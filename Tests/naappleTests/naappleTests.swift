import XCTest
@testable import naapple

final class naappleTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        XCTAssertEqual(naapple().text, "Hello, World!")
    }

    static var allTests = [
        ("testExample", testExample),
    ]
}
