import Foundation

extension FileHandle: TextOutputStream {
    public func write(_ string: String) {
        guard let data = string.data(using: .utf8) else { return }
        write(data)
    }
}

var stdout = FileHandle.standardOutput
var stderr = FileHandle.standardError

let stdin: FileHandle
switch CommandLine.arguments.count {
case ...1:
    stdin = FileHandle.standardInput
case 2 where !CommandLine.arguments[1].starts(with: "-"):
    stdin = FileHandle(forReadingAtPath: CommandLine.arguments[1])!
default:
    print("usage: scramble [PATH]", to: &stderr)
    exit(EX_USAGE)
}

var rng = PCG128S()
var puzzleFile = try! JSONDecoder().decode(PuzzleFile.self, from: stdin.readToEnd()!)
puzzleFile.scramble(using: &rng)
let encoder = JSONEncoder()
encoder.outputFormatting = [.prettyPrinted, .sortedKeys]
try! stdout.write(encoder.encode(puzzleFile))

