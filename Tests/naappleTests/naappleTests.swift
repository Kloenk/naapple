import XCTest
@testable import naapple

final class naappleTests: XCTestCase {
    func testExample() {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct
        // results.
        
        XCTAssertEqual(naapple().text, "Hello, World!")
        
        let napsterPlaylist = NANapsterPlaylist(id: "mp.175177889")
        
        let semaphore = DispatchSemaphore(value: 0)

        print("updating")
        napsterPlaylist.update { result in
            switch result {
            case .failure(let error):
                print("error: \(error)")
            case .success(_):
                print("running callback")
                semaphore.signal()
            }
        };
        
        
        
        semaphore.wait();
        print("name: \(napsterPlaylist.trackCount!)")
        
        napsterPlaylist.getTracksApi { result in
            switch result {
            case .failure(let e):
                print("error: \(e)")
            case .success(let d):
                //print("playlist: \(String(decoding: d, as: UTF8.self))")
                print("playlist_bytes: \(d.tracks.first!.name)")
            }
            
        }
        
        napsterPlaylist.getTracks(offset: 0,completion: {result in
            switch result {
            case .failure(let e):
                print("error: \(e)")
            case .success(let d):
                print("list: \(d[500].name)")
            }
            semaphore.signal()
        })
        
        semaphore.wait()
        sleep(5)
        
        
    }
    
    func napsterPlaylistName() {
        let napsterPlaylist = NANapsterPlaylist(id: "mp.175177889")
        
        print("updating")
        napsterPlaylist.update()
        
        sleep(20000)
    }

    static var allTests = [
        ("testExample", testExample),
        ("napsterPlaylistName", napsterPlaylistName),
    ]
}
