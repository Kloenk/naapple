//
//  napster.swift
//  
//
//  Created by Finn Behrens on 30.03.21.
//

import Foundation

let NANapsterApiUrl: String = "https://api.napster.com/v2.2";
let NANapsterToken: String = "YTkxZTRhNzAtODdlNy00ZjMzLTg0MWItOTc0NmZmNjU4Yzk4";


protocol NANapsterSource {
    var apikey: String { get }
    var id: String { get }
    var baseurl: String { get }
}

class NANapsterPlaylist: NANapsterSource {
    public var apikey: String = NANapsterToken
    public var id: String
    public var baseurl: String {
        NANapsterApiUrl + "/playlists/" + id
    }
    var cachedData: apiJSON?
    
    var urlSession: URLSession;
    
    public var name: String? {
        return cachedData?.playlists.first?.name
    }
    
    public var trackCount: Int? {
        return cachedData?.playlists.first?.trackCount
    }
    
    public var description: String? {
        return cachedData?.playlists.first?.description
    }
    
    public var modified: String? {
        return cachedData?.playlists.first?.modified
    }
    
    
    init(id: String, apikey: String = NANapsterToken) {
        self.id = id
        self.apikey = apikey
        
        let config = URLSessionConfiguration.default;
        config.httpAdditionalHeaders = [ "apikey" : apikey ];
        self.urlSession = URLSession(configuration: config);
    }
    
    public func update(callback: @escaping (Result<(), Error>) -> Void) {
        print("NSData: \(self.baseurl)")
        loadJson(fromURLString: self.baseurl, completion: { result in
            switch result {
            case .success(let data):
                self.parse(jsonData: data, callback: callback)
            case .failure(let error):
                print(error)
            }
        })
    }
    
    public func update() {
        update(callback: {_ in})
    }
    
    /// This is the api binding, with a limit of 200
    public func getTracksApiJson(offset: Int = 0, limit: Int = 200, completion: @escaping (Result<Data, Error>) -> Void) {
        // check input
        if cachedData == nil {
            completion(.failure(NANapsterError.NotCachedError))
            return
        }
        if limit > 200 || limit < 0 {
            completion(.failure(NANapsterError.LimitError))
            return
        }
        if offset > self.trackCount! || offset < 0 {
            completion(.failure(NANapsterError.OffsetError))
            return
        }
        
        let url = self.baseurl + "/tracks?offset=\(offset)&limit=\(limit)"
        loadJson(fromURLString: url, completion: completion)
    }
    
    public func getTracks(offset: Int = 0, limit: Int = 0, completion: @escaping (Result<Data, Error>) -> Void) {
        // check input
        if cachedData == nil {
            completion(.failure(NANapsterError.NotCachedError))
            return
        }
        let totalCount = limit != 0 ? min(self.trackCount! - offset, limit) : self.trackCount! - offset
        var missingCount = totalCount
                
        /*for x in 0...(Int(ceil(Double(totalCount) / 200)) - 1) {
            let downloadOffset = 200 * x + offset
            // TODO: implement upper limit
            print("download: \(x) (\(downloadOffset) - \(downloadOffset + 200))")
            getTracksApi(offset: downloadOffset) { result in
                switch result {
                case .failure(let e):
                    // FIXME: stop others
                    completion(.failure(e))
                    returned = true;
                case .success(let d):
                    ret.updateValue(d, forKey: x)
                }
                //semaphore.signal()
            }
        }*/
        
        
        
        // TODO: honour limit
        getTracksApi(offset: missingCount) { result in
            missingCount -= 200
            switch result {
            case .failure(let e):
                completion(.failure(e))
                return
            case .success(let d):
                print(d)
            }
        }
        
    }
    
    /// This is the api binding, with a limit of 200
    public func getTracksApi(offset: Int = 0, limit: Int = 200, completion: @escaping (Result<TracksJSON, Error>) -> Void) {
        
        getTracksApiJson(offset: offset, limit: limit) { result in
            switch result {
            case .failure(let e):
                completion(.failure(e))
                return
            case .success(let jsonData):
                do {
                    let decodedData = try JSONDecoder().decode(TracksJSON.self, from: jsonData)
                    completion(.success(decodedData))
                } catch {
                    completion(.failure(error))
                }
            }
        }
    }
    
    
    
    private func parse(jsonData: Data, callback: @escaping (Result<(), Error>) -> Void) {
        do {
            let decodedData = try JSONDecoder().decode(apiJSON.self, from: jsonData)
            self.cachedData = decodedData
            callback(.success(()))
        } catch {
            print("error: \(error)")
            callback(.failure(error))
        }
    }
    
    private func loadJson(fromURLString urlString: String, completion: @escaping (Result<Data, Error>) -> Void) {
        if let url = URL(string: urlString) {
            let urlSession = self.urlSession.dataTask(with: url) { (data, response, error) in
                if let error = error {
                    completion(.failure(error))
                }
                
                if let data = data {
                    completion(.success(data))
                }
                
            }
            urlSession.resume();
        }
    }

    
    struct apiJSON: Decodable {
        var meta: Meta
        var playlists: [Playlist]
        
        struct Playlist: Decodable {
            var type: String
            var id: String
            var name: String
            var modified: String // TODO
            var trackCount: Int
            var privacy: String
            // images
            var description: String
            // favoriteCount
            var freePlayCompliant: Bool
            // links
        }
        
    }
    
    struct TracksJSON: Decodable {
        var meta: Meta
        var tracks: [Track]
    }
    
    struct Track: Decodable {
        var type: String
        var id: String
        var index: Int
        var disc: Int
        var href: String
        var playbackSeconds: Int
        var isExplicit: Bool
        var isStreamable: Bool
        var isAvailableInHiRes: Bool
        var name: String
        /// international standart record code
        var isrc: String
        var shortcut: String
        // blurbs
        var artistId: String
        var artistName: String
        var albumName: String
        var formats: [Format]
        var losslessFormats: [Format]
        var albumId: String
        // contributors
        // links
        var previewURL: String?
        
        struct Format: Decodable {
            var type: String
            var bitrate: Int
            var name: String
            var sampleBits: Int
            var sampleRate: Int
        }
    }
    
    struct Meta: Decodable {
        var returnedCount: Int
        var totalCount: Int?
        var query: Query?
        
        struct Query: Decodable {
            var limit: Int?
            var offset: Int?
            var next: String?
            var previos: String?
        }
    }
}


enum NANapsterError: Error {
    case LimitError
    case OffsetError
    case NotCachedError
}
