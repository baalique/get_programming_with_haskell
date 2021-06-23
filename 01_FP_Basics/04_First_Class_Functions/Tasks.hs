compareLastNames name1 name2 = if result == EQ then compare (fst name1) (fst name2) else result
  where
    result = compare (snd name1) (snd name2)

--

nyOffice name = nameText ++ " NY office"
  where
    nameText = fst name ++ " " ++ snd name

sfOffice name =
  if lastName < "L"
    then nameText ++ " SF first office"
    else nameText ++ " SF second office"
  where
    lastName = snd name
    nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " Reno office"
  where
    nameText = snd name

dcOffice name = nameText ++ " DC office"
  where
    nameText = "Dear " ++ fst name ++ " " ++ snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> fst name ++ " " ++ snd name)
