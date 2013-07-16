module Testing

open Parser
open FParsec
open System

type Failure = {
    Code:  string
    Error: string
    Trace: string
}

type TestBlock = {
    Name: string
    Successes: int
    Failures:  Failure list
}

type TestData = {
    BlockName:  string
    TestBlocks: Map<string,TestBlock>
}

type TestM = TestData -> TestData

type TesterBuilder() =
    member this.Return _ = fun td -> td
    member this.Bind(tm: TestM, f:unit -> TestM): TestM = fun td -> let td' = tm td in f () td'

let tester = TesterBuilder()

let blockName n td = { td with BlockName = n }

let test str td =
    let str = str + "\n"
    let block = match Map.tryFind td.BlockName td.TestBlocks with
        | Some b -> b
        | None   -> { Name = td.BlockName; Successes = 0; Failures = [] }

    let newBlock = match parseProgram str with
        | Success _ -> { block with Successes = block.Successes + 1 }
        | Failure (msg, _, us)   ->
            { block with Failures = { Code = str; Error = msg; Trace = us.Debug.Message } :: block.Failures }

    { td with TestBlocks = td.TestBlocks.Add(td.BlockName, newBlock) }

let testMany strs td =
    Seq.fold (fun td str -> test str td) td strs


let runTester (tester:TestM) = tester { BlockName = "default"; TestBlocks = Map.empty }

let setConCol concol = Console.ForegroundColor <- concol

let cprintf color fmt =
    setConCol color
    printf fmt

let cprintfn color fmt =
    setConCol color
    printfn fmt

let printTestData td =
    let origCol = Console.ForegroundColor

    for name, block in Map.toSeq td.TestBlocks do
        let total = block.Successes + block.Failures.Length

        cprintfn ConsoleColor.Cyan "-------------------------------------------"
        cprintfn ConsoleColor.Cyan "Test Block: %s" name
        cprintfn ConsoleColor.DarkCyan "%s\n" <| "".PadRight(12+name.Length, '-')
        
        cprintfn ConsoleColor.DarkGreen "Successes: %d/%d" block.Successes total

        if block.Successes = total then
            cprintfn ConsoleColor.Green "All Passed!"

        if block.Failures.Length > 0 then
            cprintfn ConsoleColor.DarkRed "Failures:  %d\n" block.Failures.Length

            for fail in block.Failures do
                cprintfn ConsoleColor.DarkRed "-------------------------"
                cprintfn ConsoleColor.DarkRed "Code:"
                cprintfn origCol "%s" fail.Code
                cprintfn ConsoleColor.DarkRed "Error:"
                cprintfn origCol "%s" fail.Error
                cprintfn ConsoleColor.DarkRed "Trace:"
                cprintfn origCol "%s\n" fail.Trace
        
        printfn ""

    setConCol origCol
