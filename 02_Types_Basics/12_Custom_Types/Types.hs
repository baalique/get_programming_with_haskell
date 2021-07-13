import           Data.List                      ( intercalate )

type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String, String)


patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ ageHeight
 where
  name      = lname ++ ", " ++ fname
  ageHeight = " (Age: " ++ show age ++ "; height: " ++ show height ++ " sm)"

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' patient age height = name ++ ageHeight
 where
  name      = firstName patient ++ lastName patient
  ageHeight = " (Age: " ++ show age ++ "; height: " ++ show height ++ " sm)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'


data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

showSex :: Sex -> String
showSex Male   = "Male"
showSex Female = "Female"

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _               (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A  _) = True
canDonateTo (BloodType B _) (BloodType B  _) = True
canDonateTo _               _                = False

type MiddleName = String
data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l            ) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l


data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 43 188 92 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 20 180 50 (BloodType O Neg)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

data PatientF = PatientF
  { name      :: Name
  , sex       :: Sex
  , age       :: Int
  , height    :: Int
  , weight    :: Int
  , bloodType :: BloodType
  }

jackieSmith :: PatientF
jackieSmith = PatientF { name      = Name "Jackie" "Smith"
                       , age       = 18
                       , sex       = Female
                       , height    = 157
                       , weight    = 52
                       , bloodType = BloodType O Neg
                       }
jackieSmithUpdated = jackieSmith { age = 19 }

donorTo :: PatientF -> PatientF -> Bool
donorTo p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

patientSummary :: PatientF -> String
patientSummary patient = intercalate "\n" [prefix, n, s, a, h, w, bt, postfix]
 where
  prefix  = replicate 8 '*'
  n       = "Name: " ++ showName (name patient)
  s       = "Sex:" ++ showSex (sex patient)
  a       = "Age: " ++ show (age patient)
  h       = "Height: " ++ show (height patient)
  w       = "Weight: " ++ show (weight patient)
  bt      = "Blood Type: " ++ showBloodType (bloodType patient)
  postfix = replicate 8 '*'
  