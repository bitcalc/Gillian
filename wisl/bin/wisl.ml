module CLI =
  Gillian.CommandLine.Make (WSemantics.WislCMemory) (WSemantics.WislSMemory)
    (Gillian.General.External.Dummy)
    (WParserAndCompiler)
    (Gillian.Bulk.Runner.DummyRunners)

let () = CLI.main ()
