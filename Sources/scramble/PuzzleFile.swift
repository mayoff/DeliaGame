import CryptoKit
import Foundation

struct PuzzleFile: Codable {
    var puzzles: [Puzzle]
}

struct Puzzle: Codable {
    var date: String
    var author: String
    var comment: String?
    var detail: Detail

    enum Detail {
        case plain(Plain)
        case scrambled(Scrambled)
    }

    struct Plain: Codable {
        var solution: String
    }

    struct Scrambled: Codable {
        var text: String
        var hash: String
    }

    enum CodingKeys: String, CodingKey {
        case date
        case author
        case comment
    }

    struct DetailError: Error {
        var plain: Error
        var scrambled: Error
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        date = try container.decode(String.self, forKey: .date)
        author = try container.decode(String.self, forKey: .author)
        comment = try container.decodeIfPresent(String.self, forKey: .comment)
        do {
            detail = .plain(try Plain(from: decoder))
        } catch let plainError {
            do {
                detail = .scrambled(try Scrambled(from: decoder))
            } catch let scrambledError {
                throw DetailError(plain: plainError, scrambled: scrambledError)
            }
        }
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(date, forKey: .date)
        try container.encode(author, forKey: .author)
        try container.encodeIfPresent(comment, forKey: .comment)
        switch detail {
        case .plain(let plain):
            try plain.encode(to: encoder)
        case .scrambled(let scrambled):
            try scrambled.encode(to: encoder)
        }
    }
}

extension PuzzleFile {
    enum DateWarning {
        case duplicate(String)
        case gap(String)
    }

    func warnings() -> [DateWarning] {
        var dates = [String: Int]()
        for puzzle in puzzles {
            dates[puzzle.date, default: 0] += 1
        }

        var warnings = [DateWarning]()
        for (date, count) in dates {
            if count > 1 {
                warnings.append(.duplicate(date))
            }

            if dates[date.nextDate] == nil && dates[date.nextDate.nextDate] != nil {
                warnings.append(.gap(date.nextDate))
            }
        }

        return warnings
    }
}

fileprivate let dateFormatter: DateFormatter = {
    let formatter = DateFormatter()
    formatter.dateFormat = "yyyy-MM-dd"
    return formatter
}()

fileprivate let calendar = Calendar(identifier: .gregorian)

extension String {
    fileprivate var nextDate: String {
        let ints = self.split(separator: "-").map { Int($0)! }
        let myComponents = DateComponents(timeZone: TimeZone.current, era: 1, year: ints[0], month: ints[1], day: ints[2], hour: 12, minute: 0, second: 0)
        let me = calendar.date(from: myComponents)!
        let next = calendar.date(byAdding: .day, value: 1, to: me)!
        let nextComponents = calendar.dateComponents(in: TimeZone.current, from: next)
        return String(format: "%04d-%02d-%02d", nextComponents.year!, nextComponents.month!, nextComponents.day!)
    }
}

extension PuzzleFile {
    mutating func scramble<RNG: RandomNumberGenerator>(using rng: inout RNG) {
        for i in puzzles.indices {
            puzzles[i].scramble(using: &rng)
        }
    }
}

extension Puzzle {
    mutating func scramble<RNG: RandomNumberGenerator>(using rng: inout RNG) {
        switch detail {
        case .scrambled(_):
            break

        case .plain(let plain):
            let digest = SHA256.hash(data: plain.solution.data(using: .utf8)!)
            detail = .scrambled(Scrambled(
                text: plain.solution.scrambled(using: &rng),
                hash: digest.map { String(format: "%02x", $0) }.joined()
            ))
        }
    }
}

extension String {
    fileprivate func scrambled<RNG: RandomNumberGenerator>(using rng: inout RNG) -> String {
        let lowers = Set(lowercased().filter(\.isLetter))
        let vowels = Array(lowers.filter(\.isVowel)).randomMapping(using: &rng)
        let consonants = Array(lowers.filter(\.isVowel.not)).randomMapping(using: &rng)
        var mapping = Dictionary(uniqueKeysWithValues: vowels + consonants)
        for (k, v) in mapping {
            mapping[k.uppercased().first!] = v.uppercased().first!
        }

        return String(map { mapping[$0, default: $0] })
    }
}

extension Array where Element == Character {
    fileprivate func randomMapping<RNG: RandomNumberGenerator>(using rng: inout RNG) -> [(Character, Character)] {
        var values = shuffledHard(using: &rng)
        if values == self && values.count > 1 {
            values.swapAt(0, 1)
        }
        return .init(zip(self, values))
    }

    fileprivate func shuffledHard<RNG: RandomNumberGenerator>(using rng: inout RNG) -> [Character] {
        var answer: [Character] = []
        var remaining = Set(self)
        for c in dropLast() {
            let cWasRemaining = remaining.remove(c) != nil
            let r = remaining.randomElement(using: &rng)!
            answer.append(r)
            remaining.remove(r)
            if cWasRemaining {
                remaining.insert(c)
            }
        }
        answer.append(contentsOf: remaining)
        return answer
    }
}

extension Bool {
    fileprivate var not: Bool { !self }
}

extension Character {
    fileprivate var isVowel: Bool {
        switch self {
        case "a", "e", "i", "o", "u":
            return true
        default:
            return false
        }
    }
}
