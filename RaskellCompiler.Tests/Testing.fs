module Testing

open Parser
open FParsec
open System

type Failure = {
    Code:  string
    Error: string
    Trace: string
}

type Success = {
    Code: string
    Result: string
    Trace: string
}

type BlockOptions = {
    Name: string
    ShowSuccessResult: bool
    ShowSuccessTrace: bool
}

type TestBlock = {
    BlockOptions: BlockOptions
    Successes: Success list
    Failures:  Failure list
}

type TestData = {
    BlockOptions: BlockOptions
    TestBlocks: Map<string,TestBlock>
}

type TestM = TestData -> TestData

type TesterBuilder() =
    member this.Return _ = fun td -> td
    member this.Bind(tm: TestM, f:unit -> TestM): TestM = fun td -> let td' = tm td in f () td'

let tester = TesterBuilder()

let setShowSuccessResult b (td:TestData) = { td with BlockOptions = { td.BlockOptions with ShowSuccessResult = b } }
let showSuccessResult = setShowSuccessResult true
let hideSuccessResult = setShowSuccessResult false

let setShowSuccessTrace b (td:TestData) = { td with BlockOptions = { td.BlockOptions with ShowSuccessTrace = b } }
let showSuccessTrace = setShowSuccessTrace true
let hideSuccessTrace = setShowSuccessTrace false

let showSuccessAll td = showSuccessTrace td |> showSuccessResult
let hideSuccessAll td = hideSuccessTrace td |> hideSuccessResult

let blockName n (td:TestData) =
    { td with
        BlockOptions = { td.BlockOptions with
            Name = n
            ShowSuccessResult = false
            ShowSuccessTrace = false
        }
    }

let test str td =
    let str = str + "\n"
    let block = match Map.tryFind td.BlockOptions.Name td.TestBlocks with
        | Some b -> b
        | None   -> { BlockOptions = td.BlockOptions; Successes = []; Failures = [] }

    let newBlock = match parseProgram str with
        | Success (res, us, _) ->
            { block with Successes = { Code = str; Result = sprintf "%A" res; Trace = us.Debug.Message } :: block.Successes }
        | Failure (msg, _, us)   ->
            { block with Failures = { Code = str; Error = msg; Trace = us.Debug.Message } :: block.Failures }

    { td with TestBlocks = td.TestBlocks.Add(td.BlockOptions.Name, newBlock) }

let testMany strs td =
    Seq.fold (fun td str -> test str td) td strs


let runTester (tester:TestM) = tester {
    BlockOptions = {
        Name = "default"
        ShowSuccessResult = false
        ShowSuccessTrace = false
    }
    TestBlocks = Map.empty
}

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
        let total = block.Successes.Length + block.Failures.Length

        cprintfn ConsoleColor.Cyan "-------------------------------------------"
        cprintfn ConsoleColor.Cyan "Test Block: %s" name
        cprintfn ConsoleColor.DarkCyan "%s\n" <| "".PadRight(12+name.Length, '-')
        
        cprintfn ConsoleColor.DarkGreen "Successes: %d/%d" block.Successes.Length total

        if block.Successes.Length = total then
            cprintfn ConsoleColor.Green "All Passed!"

        let bo = block.BlockOptions

        if (bo.ShowSuccessResult || bo.ShowSuccessTrace) && block.Successes.Length > 0 then
            for succ in List.rev block.Successes do
                cprintfn ConsoleColor.DarkGreen "-------------------------"
                cprintfn ConsoleColor.DarkGreen "Code:"
                cprintfn origCol "%s" succ.Code

                if bo.ShowSuccessResult then
                    cprintfn ConsoleColor.DarkGreen "Result:"
                    cprintfn origCol "%s" succ.Result

                if bo.ShowSuccessTrace then
                    cprintfn ConsoleColor.DarkGreen "Trace:"
                    cprintfn origCol "%s" succ.Trace

                printfn ""
            printfn ""

        if block.Failures.Length > 0 then
            cprintfn ConsoleColor.DarkRed "Failures:  %d" block.Failures.Length

            for fail in List.rev block.Failures do
                cprintfn ConsoleColor.DarkRed "-------------------------"
                cprintfn ConsoleColor.DarkRed "Code:"
                cprintfn origCol "%s" fail.Code
                cprintfn ConsoleColor.DarkRed "Error:"
                cprintfn origCol "%s" fail.Error
                cprintfn ConsoleColor.DarkRed "Trace:"
                cprintfn origCol "%s\n" fail.Trace
        
        printfn ""

    setConCol origCol
