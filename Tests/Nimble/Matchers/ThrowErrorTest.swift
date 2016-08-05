import XCTest
import Nimble

enum NimbleError : Error {
    case laugh
    case cry
}

enum EquatableError : Error {
    case parameterized(x: Int)
}

extension EquatableError : Equatable {
}

func ==(lhs: EquatableError, rhs: EquatableError) -> Bool {
    switch (lhs, rhs) {
    case (.parameterized(let l), .parameterized(let r)):
        return l == r
    }
}

enum CustomDebugStringConvertibleError : Error {
    case a
    case b
}

extension CustomDebugStringConvertibleError : CustomDebugStringConvertible {
    var debugDescription : String {
        return "code=\(_code)"
    }
}

final class ThrowErrorTest: XCTestCase, XCTestCaseProvider {
    static var allTests: [(String, (ThrowErrorTest) -> () throws -> Void)] {
        return [
            ("testPositiveMatches", testPositiveMatches),
            ("testPositiveMatchesWithClosures", testPositiveMatchesWithClosures),
            ("testNegativeMatches", testNegativeMatches),
            ("testPositiveNegatedMatches", testPositiveNegatedMatches),
            ("testNegativeNegatedMatches", testNegativeNegatedMatches),
            ("testNegativeMatchesDoNotCallClosureWithoutError", testNegativeMatchesDoNotCallClosureWithoutError),
            ("testNegativeMatchesWithClosure", testNegativeMatchesWithClosure),
        ]
    }

    func testPositiveMatches() {
        expect { throw NimbleError.laugh }.to(throwError())
        expect { throw NimbleError.laugh }.to(throwError(NimbleError.laugh))
        expect { throw NimbleError.laugh }.to(throwError(errorType: NimbleError.self))
        expect { throw EquatableError.parameterized(x: 1) }.to(throwError(EquatableError.parameterized(x: 1)))
    }

    func testPositiveMatchesWithClosures() {
        // Generic typed closure
        expect { throw EquatableError.parameterized(x: 42) }.to(throwError { error in
            guard case EquatableError.parameterized(let x) = error else { fail(); return }
            expect(x) >= 1
        })
        // Explicit typed closure
        expect { throw EquatableError.parameterized(x: 42) }.to(throwError { (error: EquatableError) in
            guard case .parameterized(let x) = error else { fail(); return }
            expect(x) >= 1
        })
        // Typed closure over errorType argument
        expect { throw EquatableError.parameterized(x: 42) }.to(throwError(errorType: EquatableError.self) { error in
            guard case .parameterized(let x) = error else { fail(); return }
            expect(x) >= 1
        })
        // Typed closure over error argument
        expect { throw NimbleError.laugh }.to(throwError(NimbleError.laugh) { (error: Error) in
            expect(error._domain).to(beginWith("Nim"))
        })
        // Typed closure over error argument
        expect { throw NimbleError.laugh }.to(throwError(NimbleError.laugh) { (error: Error) in
            expect(error._domain).toNot(beginWith("as"))
        })
    }

    func testNegativeMatches() {
        // Same case, different arguments
        failsWithErrorMessage("expected to throw error <Parameterized(2)>, got <Parameterized(1)>") {
            expect { throw EquatableError.parameterized(x: 1) }.to(throwError(EquatableError.parameterized(x: 2)))
        }
        // Same case, different arguments
        failsWithErrorMessage("expected to throw error <Parameterized(2)>, got <Parameterized(1)>") {
            expect { throw EquatableError.parameterized(x: 1) }.to(throwError(EquatableError.parameterized(x: 2)))
        }
        // Different case
        failsWithErrorMessage("expected to throw error <Cry>, got <Laugh>") {
            expect { throw NimbleError.laugh }.to(throwError(NimbleError.cry))
        }
        // Different case with closure
        failsWithErrorMessage("expected to throw error <Cry> that satisfies block, got <Laugh>") {
            expect { throw NimbleError.laugh }.to(throwError(NimbleError.cry) { _ in return })
        }
        // Different case, implementing CustomDebugStringConvertible
        failsWithErrorMessage("expected to throw error <code=1>, got <code=0>") {
            expect { throw CustomDebugStringConvertibleError.a }.to(throwError(CustomDebugStringConvertibleError.b))
        }
    }

    func testPositiveNegatedMatches() {
        // No error at all
        expect { return }.toNot(throwError())
        // Different case
        expect { throw NimbleError.laugh }.toNot(throwError(NimbleError.cry))
    }

    func testNegativeNegatedMatches() {
        // No error at all
        failsWithErrorMessage("expected to not throw any error, got <Laugh>") {
            expect { throw NimbleError.laugh }.toNot(throwError())
        }
        // Different error
        failsWithErrorMessage("expected to not throw error <Laugh>, got <Laugh>") {
            expect { throw NimbleError.laugh }.toNot(throwError(NimbleError.laugh))
        }
    }

    func testNegativeMatchesDoNotCallClosureWithoutError() {
        failsWithErrorMessage("expected to throw error that satisfies block, got no error") {
            expect { return }.to(throwError { error in
                fail()
            })
        }
        
        failsWithErrorMessage("expected to throw error <Laugh> that satisfies block, got no error") {
            expect { return }.to(throwError(NimbleError.laugh) { error in
                fail()
            })
        }
    }

    func testNegativeMatchesWithClosure() {
#if SWIFT_PACKAGE
        let moduleName = "Nimbletest"
#else
        let moduleName = "NimbleTests"
#endif
        let innerFailureMessage = "expected to equal <foo>, got <\(moduleName).Error>"
        let closure = { (error: Error) in
            print("** In closure! With domain \(error._domain)")
            expect(error._domain).to(equal("foo"))
        }

        failsWithErrorMessage([innerFailureMessage, "expected to throw error from type <Error> that satisfies block, got <Laugh>"]) {
            expect { throw NimbleError.laugh }.to(throwError(closure: closure))
        }

        failsWithErrorMessage([innerFailureMessage, "expected to throw error <Laugh> that satisfies block, got <Laugh>"]) {
            expect { throw NimbleError.laugh }.to(throwError(NimbleError.laugh, closure: closure))
        }
    }
}
