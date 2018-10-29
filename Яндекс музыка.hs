
data MusicBand = Aquarium | LudovicoEinaudi | LanaDelRey | None deriving (Show,Eq,Ord)

data Person = Person String String MusicBand deriving Show

type Song = String

me = Person "Ekaterina" "Dubinsky" LanaDelRey
unauthorized = Person "-" "-" None

getFavouriteband:: Person->MusicBand
getFavouriteband unauthorized = error "You haven't favorite band"
getFavouriteband (Person _ _ band) = band

bands = [Aquarium, LudovicoEinaudi, LanaDelRey]
songs::[Song]
songs = ["Victory", "Life", "Cola"]
bestSongs = bands `zip` songs

members = [["BorisGrebenshikov", "MihailFajnshtejn", "DyshaRomanov", "VsevolodGakkel'"], ["composer-LudovicoEinaudi"], ["LanaDelRey"]]
bandMembers' = bands `zip` members

bandMembers unauthorized = error "You haven't favorite band"
bandMembers (Person _ _ band) = helper 0 band
 where
  helper a band = if (get bands a) == band then (get bandMembers' a) else helper (a+1) band
  get (x:xs) 0 = x
  get (x:xs) n = get xs (n-1)
