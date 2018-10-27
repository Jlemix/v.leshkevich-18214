import Data.Map as Map
data MusicBand = Interpol | Timecop1983 | BloodFlow | Perturbator | Motorama deriving (Show,Eq,Ord)

data ClassicBand = ClassicBand String String String String MusicBand deriving Show
intpol = ClassicBand "1" "2" "3" "4" Interpol

data HumanOrchestra = HumanOrchestra String MusicBand deriving Show
ptbr = HumanOrchestra "ss" Perturbator


data Person = Person String String MusicBand deriving Show
data UnPerson = UnPerson String String deriving Show

me = Person "Vladimir" "Leshkevich" BloodFlow
who = UnPerson "Vova" "Vist"
--getFavouriteband :: Person -> MusicBand
--getFavouriteband (Person _ _ band) = band


bands = [Interpol, Timecop1983, Motorama, Perturbator]

type Song = String
songs::[Song]
songs = ["The Rover", "Come back", "Fire", "Venger", "Wind In Her Hair"]
bestSongs = fromList (bands `zip` songs)

--getNextSong::Person->Song
--getNextSong (Person name lName band) = Map.lookup band bestSongs

--body = Person "heh" "heh" Blur


members = ["Paul Banks, Daniel Kessler, Sam Fogarino", "Timecop1983", "Anton Chernyak, Dmitriy Fainduhain, Sergey Krilov, Fantomas 2000", "James Kent", "Vladislav Parshin, Irina Parshina, Maxim Polivanov, Michael Nikulin"]
memfrom = fromList (bands `zip` members)


--getBandMembers (Person name lName band) = Map.lookup band memfrom
--get::(Eq a) =>[(a,b)]->a->b
--get ((key,value):xs) k = if (k == key) then value else get xs k

class User pers where
	getBandMembers::pers->Maybe String
	getNextSong:: pers->Maybe Song
	getFavouriteband::pers->MusicBand

instance User Person where
	getBandMembers (Person name lName band) = Map.lookup band memfrom
	getNextSong (Person name lName band) = Map.lookup band bestSongs
	getFavouriteband (Person _ _ band) = band


instance User UnPerson where
	getBandMembers (UnPerson _ _) = error "Sign in or sign up"
	getNextSong (UnPerson _ _) = error "Sign in or sign up"
	getFavouriteband  (UnPerson _ _) = error "Sign in or sign up"