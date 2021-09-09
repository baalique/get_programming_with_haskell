import qualified Data.Map                      as M

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateID :: Int
    , codeReview  :: Grade
    , cultureFit  :: Grade
    , education   :: Degree
    }
    deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding     = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin     = education candidate >= MS
    tests            = [passedCoding, passedCultureFit, educationMin]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readGradeDo :: IO Grade
readGradeDo = do
    input <- getLine
    let grade = read input
    return grade

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "ID:"
    cID <- readInt
    putStrLn "Code Review:"
    codeGrade <- readGrade
    putStrLn "Culture Fit:"
    cultureGrade <- readGrade
    putStrLn "Education:"
    degree <- readDegree
    return $ Candidate { candidateID = cID, codeReview = codeGrade, cultureFit = cultureGrade, education = degree }

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed    = viable candidate
    let statement = if passed then "PASSED" else "FAILED"
    return statement

candidateDB :: M.Map Int Candidate
candidateDB = M.fromList
    [ (1, Candidate { candidateID = 1, codeReview = A, cultureFit = A, education = BA })
    , (2, Candidate { candidateID = 2, codeReview = C, cultureFit = A, education = PhD })
    , (3, Candidate { candidateID = 3, codeReview = A, cultureFit = B, education = MS })
    ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cID = do
    candidate <- M.lookup cID candidateDB
    let passed    = viable candidate
    let statement = if passed then "PASSED" else "FAILED"
    return statement

candidates :: [Candidate]
candidates = map snd $ M.toList candidateDB

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed    = viable candidate
    let statement = if passed then "PASSED" else "FAILED"
    return statement

assessCandidateListMap :: [Candidate] -> [String]
assessCandidateListMap = map ((\x -> if x then "PASSED" else "FAILED") . viable)

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed    = viable candidate
    let statement = if passed then "PASSED" else "FAIL"
    return statement
