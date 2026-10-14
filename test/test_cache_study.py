import contextlib
import json
import math
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import nightly
from stats import aligned_cache_results, load_trace
import generate_report as report


def measurement(index=0, *, eviction=False, size=400):
    return {
        "name": "exec_time", "trace_exec_index": index, "input_size": size,
        "trace_source_indices": [index], "trace_source_exercise_ids": [1],
        "cache_eviction": eviction,
        "step": 1, "without_memo_step": 1, "plain_profile": [],
        "memo_profile": [["exec_cek", 30 if eviction else 60]],
        "cek_profile": [] if eviction else [["eval_cek", 100]],
        "memo_resting_adjusted_live_words": 4 if eviction else 12,
        "cek_resting_adjusted_live_words": 0 if eviction else 8,
    }


def write_results(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    footer = {"name": "memo_stats", "depth_breakdown": [], "stem_nodes": 0,
              "branch_nodes": 0, "total_nodes": 0, "hashtable_stat": [], "node_stat": []}
    path.write_text("".join(json.dumps(row) + "\n" for row in [*rows, footer]))


class CacheStudyTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.ordinary = self.root / "hazel/user1_append.json"
        self.cached = self.root / "cache/user1_append.json"
        write_results(self.ordinary, [measurement(i) for i in range(2)])
        write_results(self.cached, [measurement(i, eviction=True) for i in range(2)])

    def test_matching_smoke_prefix_is_allowed(self):
        write_results(self.cached, [measurement(eviction=True)])
        cached, reference = aligned_cache_results(self.cached, self.ordinary)
        self.assertEqual(len(cached.exec_times), 1)
        self.assertEqual(len(reference.exec_times), 1)

    def test_mismatched_inputs_or_traces_are_errors(self):
        for field, value in (("input_size", 2), ("trace_source_indices", [99]), ("trace_source_exercise_ids", [99])):
            with self.subTest(field=field):
                row = measurement(eviction=True)
                row[field] = value
                write_results(self.cached, [row])
                with self.assertRaisesRegex(ValueError, "does not align"):
                    aligned_cache_results(self.cached, self.ordinary)

    def test_rejects_duplicates_missing_metadata_and_incomplete_runs(self):
        write_results(self.cached, [measurement(eviction=True)] * 2)
        with self.assertRaisesRegex(ValueError, "trace metadata"):
            aligned_cache_results(self.cached, self.ordinary)
        row = measurement(eviction=True)
        del row["trace_source_indices"]
        write_results(self.cached, [row])
        with self.assertRaisesRegex(ValueError, "trace metadata"):
            aligned_cache_results(self.cached, self.ordinary)
        self.cached.write_text(json.dumps(measurement(eviction=True)) + "\n")
        with self.assertRaisesRegex(ValueError, "incomplete"):
            aligned_cache_results(self.cached, self.ordinary)

    def test_cache_study_must_enable_eviction_and_reference_must_disable_it(self):
        write_results(self.cached, [measurement()])
        with self.assertRaisesRegex(ValueError, "expected cache_eviction=True"):
            aligned_cache_results(self.cached, self.ordinary)
        write_results(self.ordinary, [measurement(eviction=True)])
        with self.assertRaisesRegex(ValueError, "expected cache_eviction=False"):
            load_trace(self.ordinary, eviction=False)

    def test_three_plots_use_reference_axes_and_paired_memory_overheads(self):
        with patch.object(report, "plot_scatter_for_kind", side_effect=lambda *a, **kw: kw["output_name"]) as scatter, patch.object(
            report, "plot_speedup_cdf", side_effect=lambda *a, **kw: kw["output_name"]
        ) as cdf:
            output = report.generate_cache_reports(
                modes=["user1_append"], results_dir=self.cached.parent,
                ordinary_dir=self.ordinary.parent, output_dir=self.root / "output",
            )
        self.assertEqual(scatter.call_count, 2)
        self.assertEqual(cdf.call_count, 1)
        self.assertEqual(scatter.call_args_list[0].args[0], [(100, 30), (100, 30)])
        self.assertEqual(scatter.call_args_list[1].args[0], [(60, 30), (60, 30)])
        self.assertEqual(scatter.call_args_list[0].kwargs["xlabel"], "Baseline time (ns)")
        self.assertEqual(scatter.call_args_list[1].kwargs["xlabel"], "Chordata time (ns)")
        self.assertEqual(cdf.call_args.kwargs["xlabel"], "Speedup (Baseline / Chordata (eviction))")
        self.assertTrue(all(math.isclose(value, 100 / 30) for value in cdf.call_args.args[0]))
        summary = json.loads((output.parent / "summary.json").read_text())
        self.assertAlmostEqual(summary["baseline"]["geo_mean_memory_overhead"], 0.5)
        self.assertAlmostEqual(summary["chordata"]["geo_mean_memory_overhead"], 1 / 3)
        self.assertEqual(output.read_text().count("<img "), 3)

    def test_cache_stage_runs_only_new_configuration_and_preserves_existing_data(self):
        original = self.ordinary.read_bytes()
        self.addCleanup(setattr, nightly, "RUN_OPTIONS", nightly.RUN_OPTIONS)
        nightly.RUN_OPTIONS = nightly.RunOptions(locked=True)

        def execute(command):
            self.assertEqual(command[6:8], ["cache", "user1_append"])
            self.assertEqual(command[8:10], ["400", "2"])
            write_results(Path(command[-1]), [measurement(i, eviction=True) for i in range(2)])

        with contextlib.chdir(self.root), patch.object(nightly, "hazel_modes", ("user1_append",)), patch.object(
            nightly, "_result_path_for_mode", return_value=self.ordinary
        ), patch.object(nightly, "ensure_switch"), patch.object(nightly, "generate_ml_files"), patch.object(
            nightly, "opam_exec", side_effect=execute
        ) as run, patch.object(nightly, "run_modes") as ordinary, patch.object(nightly, "run_compare_modes") as hazel:
            nightly.cache_project(generate_report=False)
        run.assert_called_once()
        ordinary.assert_not_called()
        hazel.assert_not_called()
        self.assertEqual(self.ordinary.read_bytes(), original)


if __name__ == "__main__":
    unittest.main()
