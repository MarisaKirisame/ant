type m_crc32c = Int64.t

external m_crc32c_unit : unit -> m_crc32c = "m_crc32c_unit_stub"

external m_crc32c_mul : m_crc32c -> m_crc32c -> m_crc32c = "m_crc32c_mul_stub"

external m_crc32c_from_int : int -> m_crc32c = "m_crc32c_from_int_stub"

external m_crc32c_from_char : char -> m_crc32c = "m_crc32c_from_char_stub"

external m_crc32c_hash : m_crc32c -> int = "m_crc32c_hash_stub"

