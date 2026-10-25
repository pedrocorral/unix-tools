-- sha256/main.rg — BSD sha256(1) cryptographic digest, HMAC-SHA256 & Argon2id utility in Pure Ridgun
-- @implements FR-SEC-002

import sec_lib

fn :: drop prog [String] => [String]
      drop prog []       =  []
      drop prog (_:rest) =  rest

fn :: read sha stdin chunk String String => String
      read sha stdin chunk acc "" = acc
      read sha stdin chunk acc chunk = read sha stdin loop (acc + chunk)

fn :: read sha stdin loop String => String
      read sha stdin loop acc    =
        read sha stdin chunk acc (posix read 0 65536)

fn :: read stdin if needed Sha256Spec => String
      read stdin if needed spec       =
        | (str len (sh_check_file spec)) > 0     = ""
        | sh_has_str spec                        = ""
        | (sec str list len (sh_files spec)) > 0 = ""
        | otherwise                              = read sha stdin loop ""

fn :: finish sha256 (Int, String) => ()
      finish sha256 (0, out)      =  echo out
      finish sha256 (rc, out)     =
        _ = echo out
        sys exit rc

fn :: main => ()
      main =
        args = drop prog (cli args)
        spec = parse sha256 args args
        stdin_data = read stdin if needed spec
        res = evaluate sha256 spec with stdin_data
        finish sha256 res
