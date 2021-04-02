struct naapple {
    var text = "Hello, World!"
}


protocol NATrack {
    var name: String { get }
    var artistName: String { get }
    var albumName: String { get }
    var isrc: String? { get }
}
