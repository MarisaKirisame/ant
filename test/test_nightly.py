import contextlib
import io
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import nightly


class CiPipelineTests(unittest.TestCase):
    def setUp(self):
        self.addCleanup(setattr, nightly, "RUN_OPTIONS", nightly.RUN_OPTIONS)

    def test_locked_dependencies_do_not_update_or_install_editor_tools(self):
        with patch.object(nightly, "ensure_switch"), patch.object(nightly, "run") as run:
            self.assertEqual(nightly.main(["dependency", "--locked"]), 0)
        commands = [call.args[0] for call in run.call_args_list]
        self.assertTrue(any(command[:2] == ["opam", "install"] for command in commands))
        for command in commands:
            self.assertNotIn("update", command)
            self.assertNotIn("upgrade", command)
            self.assertNotIn("lock", command)
            self.assertNotIn("ocaml-lsp-server", command)
            self.assertNotIn("ocamlformat", command)

    def test_locked_dependencies_require_a_lock_before_installing(self):
        with tempfile.TemporaryDirectory() as directory:
            with patch.object(nightly, "REPO_ROOT", Path(directory)), patch.object(nightly, "run") as run:
                with self.assertRaisesRegex(FileNotFoundError, "dune.lock/lock.dune"):
                    nightly.install_dependencies(locked=True)
                run.assert_not_called()

    def test_prepared_pipeline_keeps_all_experiments(self):
        stages = (
            "clean_results", "clean_output", "install_dependencies", "hazel_dependency",
            "build_project", "arith_project", "hazel_experiment_project",
            "cache_project", "scaling_project", "entropy_scaling_project",
        )
        with contextlib.ExitStack() as stack:
            mocks = {name: stack.enter_context(patch.object(nightly, name)) for name in stages}
            report = stack.enter_context(patch.object(nightly.report_module, "generate_reports"))
            self.assertEqual(nightly.main(["all", "--locked", "--skip-dependencies"]), 0)
            for name, mock in mocks.items():
                if name in ("install_dependencies", "hazel_dependency"):
                    mock.assert_not_called()
                else:
                    self.assertEqual(mock.call_count, 1, name)
            report.assert_called_once_with()
            self.assertTrue(nightly.RUN_OPTIONS.locked)

    def test_skip_dependencies_rejects_other_stages(self):
        with patch.object(nightly, "install_dependencies") as install, contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(nightly.main(["dependency", "--skip-dependencies"]), 1)
            install.assert_not_called()

    def test_hazel_install_does_not_build_before_cache_save(self):
        with patch.object(nightly, "install_hazel_dependencies") as install, patch.object(nightly, "build_hazel") as build:
            self.assertEqual(nightly.main(["hazel-install"]), 0)
            install.assert_called_once_with()
            build.assert_not_called()
            self.assertEqual(nightly.main(["hazel-build"]), 0)
            build.assert_called_once_with()
            self.assertEqual(install.call_count, 1)

    def test_locked_benchmarks_do_not_invoke_formatter(self):
        for locked in (False, True):
            with self.subTest(locked=locked):
                nightly.RUN_OPTIONS = nightly.RunOptions(locked=locked)
                with contextlib.ExitStack() as stack:
                    for name in ("ensure_switch", "generate_ml_files", "_remove_result_files_for_modes", "_remove_compare_result_files_for_modes"):
                        stack.enter_context(patch.object(nightly, name))
                    execute = stack.enter_context(patch.object(nightly, "opam_exec"))
                    nightly.run_modes(())
                    nightly.run_compare_modes(())
                    nightly._prepare_scaling_run()
                    format_calls = [call for call in execute.call_args_list if call.args[0] == ["dune", "fmt"]]
                    self.assertEqual(len(format_calls), 0 if locked else 3)


if __name__ == "__main__":
    unittest.main()
