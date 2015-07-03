module MusicLibrary where

type Title = String
type Album = String
type Genre = String
type Length = String

data Song = Song {
    songTitle :: Title,
    songAlbum :: Album,
    songAuthor :: Author,
    songGenre :: Genre,
    songLength :: Length
} deriving (Show, Eq)

type Name = String
type Birthyear = Int
type Recordlabel = String

data Author = Author {
    authorName :: Name,
    authorBirthyear :: Birthyear,
    authorRecordlabel :: Recordlabel
} deriving (Show, Eq)

myAuthor = Author { authorName = "John Doe", authorBirthyear = 1999, authorRecordlabel = "Some Label" }
myAuthor2 = Author { authorName = "Foo Bar", authorBirthyear = 1999, authorRecordlabel = "Some Label" }

mySong = Song { songTitle = "Some Title", songAlbum = "Some Album", songAuthor = myAuthor, songGenre = "Some Genre", songLength = "3:33"}
mySong2 = Song { songTitle = "Some Title 2", songAlbum = "Some Album", songAuthor = myAuthor2, songGenre = "Some Genre", songLength = "3:34"}


addSong :: Song -> [Song] -> [Song]
addSong song xs | filter (== song) xs == [] = song : xs
                | otherwise = error "Duplicate song"

removeSong :: Song -> [Song] -> [Song]
removeSong song (x:xs) | song == x = removeSong song xs
                       | otherwise = x : removeSong song xs
removeSong song [] = []

removeByAuthor :: Author -> [Song] -> [Song]
removeByAuthor auth (x:xs) | songAuthor x == auth = removeByAuthor auth xs
                           | otherwise = x : removeByAuthor auth xs
removeByAuthor _ [] = []

songByTitle :: Title -> [Song] -> Song
songByTitle title (x:xs) | title == songTitle x = x
                         | otherwise = songByTitle title xs
songByTitle _ [] = error "Song not found"

songByAlbum :: Album -> [Song] -> [Song]
songByAlbum album (x:xs) | album == songAlbum x = x : songByAlbum album xs
                         | otherwise = songByAlbum album xs
songByAlbum _ [] = []

songByAuthor :: Author -> [Song] -> [Song]
songByAuthor author (x:xs) | author == songAuthor x =
                            x : songByAuthor author xs
                           | otherwise = songByAuthor author xs
songByAuthor _ [] = []

authorInfo :: Author -> [Author] -> String
authorInfo auth (x:xs) | x == auth = show auth
                       | otherwise = authorInfo auth xs
authorInfo _ [] = error "Author not found"
