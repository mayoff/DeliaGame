// http://www.pcg-random.org/pdf/toms-oneill-pcg-family.pdf
// PCG-XSL-RR 128

// https://github.com/imneme/pcg-c/blob/master/include/pcg_variants.h
// pcg64s_random_r aka pcg_oneseq_128_xsl_rr_64_random_r

public typealias PCG128S = PCG_XSL_RR_128

/*
 A Swift existential container has inline storage for a three-word value, as documented here:
 https://github.com/apple/swift/blob/master/docs/ABI/TypeLayout.rst

 This RNG requires 128 bits of storage, so it fits comfortably in the container (on 64-bit platforms). I could not easily find a popular 192-bit RNG algorithm.
 */
@frozen
public struct PCG_XSL_RR_128: RandomNumberGenerator {
    @inlinable public mutating func next() -> UInt64 {
        state = state &* multiplier &+ increment
        return (state.low ^ state.high).rotateRight(state.high &>> 58)
    }

    @inlinable public init(seed: State = .init(high: 123, low: 456)) {
        state = 0
        _ = next()
        state = state &+ seed
        _ = next()
    }

    @inlinable public init(state: State) {
        self.state = state
    }

    public mutating func advance(by offset: UInt128) {
        var offset = offset
        var accMult: UInt128 = 1
        var accPlus: UInt128 = 0
        var curMult = multiplier
        var curPlus = increment
        while !offset.isZero {
            if offset.low & 1 == 1 {
                accMult = accMult &* curMult
                accPlus = accPlus &* curMult &+ curPlus
            }
            curPlus = (curMult &+ 1) &* curPlus
            curMult = curMult &* curMult
            offset = offset.half
        }

        state = accMult &* state &+ accPlus
    }

    public typealias State = UInt128
    public var state: State

    @usableFromInline var multiplier: UInt128 { return .init(high: 2549297995355413924, low: 4865540595714422341) }
    @usableFromInline var increment: UInt128 { return .init(high: 6364136223846793005, low: 1442695040888963407) }
}

extension PCG_XSL_RR_128 {
    @frozen public struct UInt128: ExpressibleByIntegerLiteral, Hashable, Codable {
        public var low: UInt64
        public var high: UInt64

        @inlinable public init(high: UInt64, low: UInt64) {
            self.low = low
            self.high = high
        }

        @inlinable public init(_ pair: (high: UInt64, low: UInt64)) {
            self.low = pair.low
            self.high = pair.high
        }

        @inlinable public init(integerLiteral: UInt64) {
            self.low = integerLiteral
            self.high = 0
        }

        @inlinable public init(bitPattern: Int) {
            self.low = UInt64(bitPattern: Int64(bitPattern))
            self.high = (bitPattern < 0) ? .max : 0
        }

        @inlinable public static func &*(lhs: UInt128, rhs: UInt128) -> UInt128 {
            // ((lhs.high << 64 + lhs.low) * (rhs.high << 64 + rhs.low)) & (1 << 128 - 1)
            // = ((lhs.high << 64) * (rhs.high << 64) + lhs.low * rhs.high << 64 + lhs.high * rhs.low << 64 + lhs.low * rhs.low) & (1 << 128 - 1)
            // = (lhs.high * rhs.high << 128 + lhs.low * rhs.high << 64 + lhs.high * rhs.low << 64 + lhs.low * rhs.low) & (1 << 128 - 1)
            // = (lhs.high * rhs.high << 128) & (1 << 128 - 1) + (lhs.low * rhs.high << 64) & (1 << 128 - 1) + (lhs.high * rhs.low << 64) & (1 << 128 - 1) + (lhs.low * rhs.low) & (1 << 128 - 1)
            // = 0 + ((lhs.low * rhs.high).low << 64) & (1 << 128 - 1) + ((lhs.high * rhs.low).low << 64) & (1 << 128 - 1) + (lhs.low * rhs.low) & (1 << 128 - 1)
            // = (((lhs.low * rhs.high).low  + (lhs.high * rhs.low).low) << 64) & (1 << 128 - 1) + (lhs.low * rhs.low) & (1 << 128 - 1)
            // = ((lhs.low &* rhs.high + lhs.high &* rhs.low) << 64) & (1 << 128 - 1) + (lhs.low * rhs.low) & (1 << 128 - 1)
            // = ((lhs.low &* rhs.high + lhs.high &* rhs.low) << 64 + lhs.low * rhs.low) & (1 << 128 - 1)
            return UInt128(high: (lhs.low &* rhs.high &+ lhs.high &* rhs.low), low: 0) &+ UInt128(lhs.low.multipliedFullWidth(by: rhs.low))
        }

        @inlinable public static func &+(lhs: UInt128, rhs: UInt128) -> UInt128 {
            let (partialValue: sumLow, overflow: carry) = lhs.low.addingReportingOverflow(rhs.low)
            return .init(high: lhs.high &+ rhs.high &+ (carry ? 1 : 0), low: sumLow)
        }

        @inlinable public var half: UInt128 {
            var half = self
            half.low &>>= 1
            if half.high & 1 == 1 { half.low |= (1 &<< 63) }
            half.high &>>= 1
            return half
        }

        @inlinable public var isZero: Bool {
            return low == 0 && high == 0
        }

        @inlinable public var negated: UInt128 {
            var negated = self
            negated.low ^= .max
            negated.high ^= .max
            negated = negated &+ 1
            return negated
        }
    }
}

extension UInt64 {
    @usableFromInline func rotateRight(_ count: UInt64) -> UInt64 {
        return (self &>> count) | (self &<< (64 &- count))
    }
}
