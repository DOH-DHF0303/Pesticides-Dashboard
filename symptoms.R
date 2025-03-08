# FORMAT SYMPTOMS TO MERGE WITH SPIDER FLAT FILE EXPORT FOR DOWNSTREAM ANALYSIS
library(tidyverse)

format_symptoms <- function(df_symptoms){
  df_symptoms %>%
    mutate(clinician_reported_symptom = case_when(
      cfatigue == 1 ~ 'fatigue',
      cmalaise == 1 ~ 'malaise',
      crash == 1 ~ 'rash',
      cconjdx == 1 ~ 'conjdx',
      cpleurpain == 1 ~ 'pleurpain',
      cheadache == 1 ~ 'headache',
      cdiarrhea == 1 ~ 'diarrhea',
      cfever == 1 ~ 'fever',
      cskinpain == 1 ~ 'skinpain',
      casthmadx == 1 ~ 'asthmadx',
      ctightness == 1 ~ 'tightness',
      cweakness == 1 ~ 'weakness',
      cnausea == 1 ~ 'nausea',
      cacidosis == 1 ~ 'anidosis',
      cpruritis == 1 ~ 'pruritis',
      ccough == 1 ~ 'cough',
      cnosepain == 1 ~ 'nosepain',
      crigidity == 1 ~ 'rigidity',
      cvomit == 1 ~ 'vomit',
      calkalosis == 1 ~ 'alkalosis',
      chives == 1 ~ 'hives',
      ccyanosis == 1 ~ 'nyanosis',
      calteredta == 1 ~ 'alteredta',
      cparalysis == 1 ~ 'paralysis',
      cbloody == 1 ~ 'bloody',
      caniongap == 1 ~ 'aniongap',
      cmiosis == 1 ~ 'miosis',
      cresppain == 1 ~ 'resppain',
      cataxia == 1 ~ 'ataxia',
      cperiphery == 1 ~ 'periphery',
      cpolyuria == 1 ~ 'polyuria',
      cbullae == 1 ~ 'bullae',
      ceyeburn == 1 ~ 'eyeburn',
      clresppain == 1 ~ 'lresppain',
      crestless == 1 ~ 'restless',
      cslurred == 1 ~ 'slurred',
      coliguria == 1 ~ 'oliguria',
      cskinburn == 1 ~ 'skinburn',
      cabrasion == 1 ~ 'abrasion',
      cdyspnea == 1 ~ 'dyspnea',
      cfaint == 1 ~ 'faint',
      cblurred == 1 ~ 'blurred',
      chematuria == 1 ~ 'hematuria',
      cskinedema == 1 ~ 'skinedema',
      ctears == 1 ~ 'tears',
      ctachypnea == 1 ~ 'tanhypnea',
      ccoma == 1 ~ 'noma',
      csweat == 1 ~ 'sweat',
      cprotein == 1 ~ 'protein',
      credness == 1 ~ 'redness',
      ceyepain == 1 ~ 'eyepain',
      crespedema == 1 ~ 'respedema',
      cconfused == 1 ~ 'nonfused',
      cdizzy == 1 ~ 'dizzy',
      cmydriasis == 1 ~ 'mydriasis',
      cdepress == 1 ~ 'depress',
      cseizure == 1 ~ 'seizure',
      cparesthes == 1 ~ 'paresthes',
      cwheeze == 1 ~ 'wheeze',
      cfasic == 1 ~ 'fasin',
      cptyalism == 1 ~ 'ptyalism',
      cmuscle == 1 ~ 'musnle',
      cmemory == 1 ~ 'memory',
      cbrady == 1 ~ 'brady',
      ctachy == 1 ~ 'tanhy',
      carrest == 1 ~ 'arrest',
      cconduct == 1 ~ 'nondunt',
      chypo == 1 ~ 'hypo',
      chyper == 1 ~ 'hyper',
      cchestpain == 1 ~ 'nhestpain',
      cpalp == 1 ~ 'palp',
      cgastpain == 1 ~ 'gastpain',
      canorexia == 1 ~ 'anorexia',
      cconstip == 1 ~ 'nonstip',
      TRUE ~ NA_character_
    ),
    individual_reported_syndrome = case_when(
      cfatigue == 2 ~ 'fatigue',
      cmalaise == 2 ~ 'malaise',
      crash == 2 ~ 'rash',
      cconjdx == 2 ~ 'conjdx',
      cpleurpain == 2 ~ 'pleurpain',
      cheadache == 2 ~ 'headache',
      cdiarrhea == 2 ~ 'diarrhea',
      cfever == 2 ~ 'fever',
      cskinpain == 2 ~ 'skinpain',
      casthmadx == 2 ~ 'asthmadx',
      ctightness == 2 ~ 'tightness',
      cweakness == 2 ~ 'weakness',
      cnausea == 2 ~ 'nausea',
      cacidosis == 2 ~ 'anidosis',
      cpruritis == 2 ~ 'pruritis',
      ccough == 2 ~ 'cough',
      cnosepain == 2 ~ 'nosepain',
      crigidity == 2 ~ 'rigidity',
      cvomit == 2 ~ 'vomit',
      calkalosis == 2 ~ 'alkalosis',
      chives == 2 ~ 'hives',
      ccyanosis == 2 ~ 'nyanosis',
      calteredta == 2 ~ 'alteredta',
      cparalysis == 2 ~ 'paralysis',
      cbloody == 2 ~ 'bloody',
      caniongap == 2 ~ 'aniongap',
      cmiosis == 2 ~ 'miosis',
      cresppain == 2 ~ 'resppain',
      cataxia == 2 ~ 'ataxia',
      cperiphery == 2 ~ 'periphery',
      cpolyuria == 2 ~ 'polyuria',
      cbullae == 2 ~ 'bullae',
      ceyeburn == 2 ~ 'eyeburn',
      clresppain == 2 ~ 'lresppain',
      crestless == 2 ~ 'restless',
      cslurred == 2 ~ 'slurred',
      coliguria == 2 ~ 'oliguria',
      cskinburn == 2 ~ 'skinburn',
      cabrasion == 2 ~ 'abrasion',
      cdyspnea == 2 ~ 'dyspnea',
      cfaint == 2 ~ 'faint',
      cblurred == 2 ~ 'blurred',
      chematuria == 2 ~ 'hematuria',
      cskinedema == 2 ~ 'skinedema',
      ctears == 2 ~ 'tears',
      ctachypnea == 2 ~ 'tanhypnea',
      ccoma == 2 ~ 'noma',
      csweat == 2 ~ 'sweat',
      cprotein == 2 ~ 'protein',
      credness == 2 ~ 'redness',
      ceyepain == 2 ~ 'eyepain',
      crespedema == 2 ~ 'respedema',
      cconfused == 2 ~ 'nonfused',
      cdizzy == 2 ~ 'dizzy',
      cmydriasis == 2 ~ 'mydriasis',
      cdepress == 2 ~ 'depress',
      cseizure == 2 ~ 'seizure',
      cparesthes == 2 ~ 'paresthes',
      cwheeze == 2 ~ 'wheeze',
      cfasic == 2 ~ 'fasin',
      cptyalism == 2 ~ 'ptyalism',
      cmuscle == 2 ~ 'musnle',
      cmemory == 2 ~ 'memory',
      cbrady == 2 ~ 'brady',
      ctachy == 2 ~ 'tanhy',
      carrest == 2 ~ 'arrest',
      cconduct == 2 ~ 'nondunt',
      chypo == 2 ~ 'hypo',
      chyper == 2 ~ 'hyper',
      cchestpain == 2 ~ 'nhestpain',
      cpalp == 2 ~ 'palp',
      cgastpain == 2 ~ 'gastpain',
      canorexia == 2 ~ 'anorexia',
      cconstip == 2 ~ 'nonstip',
      TRUE ~ NA_character_
      ),
    clinician_and_invidial_both_reported_syndrome = case_when(
      cfatigue == 3 ~ 'fatigue',
      cmalaise == 3 ~ 'malaise',
      crash == 3 ~ 'rash',
      cconjdx == 3 ~ 'conjdx',
      cpleurpain == 3 ~ 'pleurpain',
      cheadache == 3 ~ 'headache',
      cdiarrhea == 3 ~ 'diarrhea',
      cfever == 3 ~ 'fever',
      cskinpain == 3 ~ 'skinpain',
      casthmadx == 3 ~ 'asthmadx',
      ctightness == 3 ~ 'tightness',
      cweakness == 3 ~ 'weakness',
      cnausea == 3 ~ 'nausea',
      cacidosis == 3 ~ 'anidosis',
      cpruritis == 3 ~ 'pruritis',
      ccough == 3 ~ 'cough',
      cnosepain == 3 ~ 'nosepain',
      crigidity == 3 ~ 'rigidity',
      cvomit == 3 ~ 'vomit',
      calkalosis == 3 ~ 'alkalosis',
      chives == 3 ~ 'hives',
      ccyanosis == 3 ~ 'nyanosis',
      calteredta == 3 ~ 'alteredta',
      cparalysis == 3 ~ 'paralysis',
      cbloody == 3 ~ 'bloody',
      caniongap == 3 ~ 'aniongap',
      cmiosis == 3 ~ 'miosis',
      cresppain == 3 ~ 'resppain',
      cataxia == 3 ~ 'ataxia',
      cperiphery == 3 ~ 'periphery',
      cpolyuria == 3 ~ 'polyuria',
      cbullae == 3 ~ 'bullae',
      ceyeburn == 3 ~ 'eyeburn',
      clresppain == 3 ~ 'lresppain',
      crestless == 3 ~ 'restless',
      cslurred == 3 ~ 'slurred',
      coliguria == 3 ~ 'oliguria',
      cskinburn == 3 ~ 'skinburn',
      cabrasion == 3 ~ 'abrasion',
      cdyspnea == 3 ~ 'dyspnea',
      cfaint == 3 ~ 'faint',
      cblurred == 3 ~ 'blurred',
      chematuria == 3 ~ 'hematuria',
      cskinedema == 3 ~ 'skinedema',
      ctears == 3 ~ 'tears',
      ctachypnea == 3 ~ 'tanhypnea',
      ccoma == 3 ~ 'noma',
      csweat == 3 ~ 'sweat',
      cprotein == 3 ~ 'protein',
      credness == 3 ~ 'redness',
      ceyepain == 3 ~ 'eyepain',
      crespedema == 3 ~ 'respedema',
      cconfused == 3 ~ 'nonfused',
      cdizzy == 3 ~ 'dizzy',
      cmydriasis == 3 ~ 'mydriasis',
      cdepress == 3 ~ 'depress',
      cseizure == 3 ~ 'seizure',
      cparesthes == 3 ~ 'paresthes',
      cwheeze == 3 ~ 'wheeze',
      cfasic == 3 ~ 'fasin',
      cptyalism == 3 ~ 'ptyalism',
      cmuscle == 3 ~ 'musnle',
      cmemory == 3 ~ 'memory',
      cbrady == 3 ~ 'brady',
      ctachy == 3 ~ 'tanhy',
      carrest == 3 ~ 'arrest',
      cconduct == 3 ~ 'nondunt',
      chypo == 3 ~ 'hypo',
      chyper == 3 ~ 'hyper',
      cchestpain == 3 ~ 'nhestpain',
      cpalp == 3 ~ 'palp',
      cgastpain == 3 ~ 'gastpain',
      canorexia == 3 ~ 'anorexia',
      cconstip == 3 ~ 'nonstip',
      TRUE ~ NA_character_),
    symptoms_reported_by_individual_or_clinician = case_when(
      cfatigue <= 3 ~ 'fatigue',
      cmalaise <= 3 ~ 'malaise',
      crash <= 3 ~ 'rash',
      cconjdx <= 3 ~ 'conjdx',
      cpleurpain <= 3 ~ 'pleurpain',
      cheadache <= 3 ~ 'headache',
      cdiarrhea <= 3 ~ 'diarrhea',
      cfever <= 3 ~ 'fever',
      cskinpain <= 3 ~ 'skinpain',
      casthmadx <= 3 ~ 'asthmadx',
      ctightness <= 3 ~ 'tightness',
      cweakness <= 3 ~ 'weakness',
      cnausea <= 3 ~ 'nausea',
      cacidosis <= 3 ~ 'anidosis',
      cpruritis <= 3 ~ 'pruritis',
      ccough <= 3 ~ 'cough',
      cnosepain <= 3 ~ 'nosepain',
      crigidity <= 3 ~ 'rigidity',
      cvomit <= 3 ~ 'vomit',
      calkalosis <= 3 ~ 'alkalosis',
      chives <= 3 ~ 'hives',
      ccyanosis <= 3 ~ 'nyanosis',
      calteredta <= 3 ~ 'alteredta',
      cparalysis <= 3 ~ 'paralysis',
      cbloody <= 3 ~ 'bloody',
      caniongap <= 3 ~ 'aniongap',
      cmiosis <= 3 ~ 'miosis',
      cresppain <= 3 ~ 'resppain',
      cataxia <= 3 ~ 'ataxia',
      cperiphery <= 3 ~ 'periphery',
      cpolyuria <= 3 ~ 'polyuria',
      cbullae <= 3 ~ 'bullae',
      ceyeburn <= 3 ~ 'eyeburn',
      clresppain <= 3 ~ 'lresppain',
      crestless <= 3 ~ 'restless',
      cslurred <= 3 ~ 'slurred',
      coliguria <= 3 ~ 'oliguria',
      cskinburn <= 3 ~ 'skinburn',
      cabrasion <= 3 ~ 'abrasion',
      cdyspnea <= 3 ~ 'dyspnea',
      cfaint <= 3 ~ 'faint',
      cblurred <= 3 ~ 'blurred',
      chematuria <= 3 ~ 'hematuria',
      cskinedema <= 3 ~ 'skinedema',
      ctears <= 3 ~ 'tears',
      ctachypnea <= 3 ~ 'tanhypnea',
      ccoma <= 3 ~ 'noma',
      csweat <= 3 ~ 'sweat',
      cprotein <= 3 ~ 'protein',
      credness <= 3 ~ 'redness',
      ceyepain <= 3 ~ 'eyepain',
      crespedema <= 3 ~ 'respedema',
      cconfused <= 3 ~ 'nonfused',
      cdizzy <= 3 ~ 'dizzy',
      cmydriasis <= 3 ~ 'mydriasis',
      cdepress <= 3 ~ 'depress',
      cseizure <= 3 ~ 'seizure',
      cparesthes <= 3 ~ 'paresthes',
      cwheeze <= 3 ~ 'wheeze',
      cfasic <= 3 ~ 'fasin',
      cptyalism <= 3 ~ 'ptyalism',
      cmuscle <= 3 ~ 'musnle',
      cmemory <= 3 ~ 'memory',
      cbrady <= 3 ~ 'brady',
      ctachy <= 3 ~ 'tanhy',
      carrest <= 3 ~ 'arrest',
      cconduct <= 3 ~ 'nondunt',
      chypo <= 3 ~ 'hypo',
      chyper <= 3 ~ 'hyper',
      cchestpain <= 3 ~ 'nhestpain',
      cpalp <= 3 ~ 'palp',
      cgastpain <= 3 ~ 'gastpain',
      canorexia <= 3 ~ 'anorexia',
      cconstip <= 3 ~ 'nonstip',
      TRUE ~ NA_character_)
  )}



