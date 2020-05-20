#!/usr/bin/env ocaml

#use "topfind";;
#thread

#require "shexp.process";;
#use "./githooks/common.ml";;
#require "fileutils"

let install hook =
  let absdest = Filename.concat destination hook in
  let absorg = Filename.concat githooksFolder hook in
  Printf.printf "%s -> %s" absorg absdest;
  FileUtil.cp [ absorg ] absdest;
  Unix.chmod absdest ((Unix.stat absdest).st_perm + 0o100) 
  

let () = Stdlib.List.iter install hooks_names