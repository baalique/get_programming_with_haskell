import qualified Data.Map                      as M

type Username = String
type UserID = Int
type Credits = Int
type CompanyID = Int

userNameDB :: M.Map UserID Username
userNameDB = M.fromList
    [(1, "username1"), (2, "username2"), (3, "username3"), (4, "username4"), (5, "username5"), (6, "username6")]

creditsDB :: M.Map Username Credits
creditsDB = M.fromList
    [ ("username1", 2000)
    , ("username2", 15000)
    , ("username3", 300)
    , ("username4", 12)
    , ("username5", 50000)
    , ("username6", 150000)
    ]

lookupUsername :: UserID -> Maybe Username
lookupUsername id = M.lookup id userNameDB

lookupCredits :: Username -> Maybe Credits
lookupCredits username = M.lookup username creditsDB

altLookupCredits :: Maybe Username -> Maybe Credits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromID :: UserID -> Maybe Credits
creditsFromID id = lookupUsername id >>= lookupCredits

userIDDB :: M.Map CompanyID UserID
userIDDB = M.fromList [(1001, 1), (1002, 2), (1003, 3), (1004, 4), (1005, 5), (1006, 6)]

lookupUserID :: CompanyID -> Maybe UserID
lookupUserID id = M.lookup id userIDDB

creditsFromCompanyID :: CompanyID -> Maybe Credits
creditsFromCompanyID id = lookupUserID id >>= lookupUsername >>= lookupCredits
