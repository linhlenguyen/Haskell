module PriorLearn(Answer(..),priorLearnData,field,name) where

import List

data Answer = Ans
  { edPlan1 :: Int
  , highDegree2 :: Int
  , howLikelyAreYouToCompleteBachelorsDegreeAtPsu3 :: Int
  , why_AdvanceJob4 :: Int
  , why_BetterJob5 :: Int
  , why_BetterInformed6 :: Int
  , why_GeneralEd7 :: Int
  , why_PrepForGrad8:: Int
  , why_Parents9 :: Int
  , why_MeetPeople10 :: Int
  , why_EarnMoney11 :: Int
  , psu_Cost12 :: Int
  , psu_Location13 :: Int
  , psu_Majors14 :: Int
  , psu_AdmRequirements15 :: Int
  , psu_AbleToLiveAtHome16 :: Int
  , psu_Reputation17 :: Int
  , psu_Athletics18 :: Int
  , psu_Academics19 :: Int
  , psu_UnivStudies20 :: Int
  , psu_CommBasedLearn21 :: Int
  , psu_FamilyRecomend22 :: Int
  , psu_FriendsAttend23 :: Int
  , psu_DiverseStudentBody24 :: Int
  , psu_FinancialAid25 :: Int
  , advising_NewStudOrient26 :: Int
  , advising_UASCAppointment27 :: Int
  , advising_UASCworkshop28 :: Int
  , advising_OtherPsuAdvisor29 :: Int
  , advising_Other30 :: Int
  , understandGradRequirements31 :: Int
  , frinqRegThemeInterest32 :: Int
  , frinqRegTime33 :: Int
  , topConcernBeginYearAtPsu34 :: Int
  , concernClass_NotSmartEnough35 :: Int
  , concernClass_MightFail36 :: Int
  , concernClass_KeepUpWithWork37 :: Int
  , concernClass_MakeFriends38 :: Int
  , concernClass_TooManyCredits39 :: Int
  , concernClass_Other40 :: Int
  , major41 :: String
  , expectedCreditsPerTerm42 :: Int
  , workHoursThisYear43 :: Int
  , workLocation44 :: Int
  , computer_Home45 :: Int
  , computer_Work46 :: Int
  , computer_PsuLab47 :: Int
  , computer_PublicLibrary48 :: Int
  , computer_FriendOrFamily49 :: Int
  , computer_NoAccess50 :: Int
  , computer_Other51:: Int
  , dependents52 :: Int
  , financialAid53 :: Int
  , transportation_Walk54 :: Int
  , transportation_Drive55 :: Int
  , transportation_Carpool56 :: Int
  , transportation_Bike57 :: Int
  , transportation_PublicTrans58 :: Int
  , housingType59 :: Int
  , part_StudentClub60 :: Int
  , part_VolInComm61 :: Int
  , part_StudentGovt62 :: Int
  , part_GreekLife63 :: Int
  , part_Intramural64 :: Int
  , part_OutdoorProgram65 :: Int
  , part_ResidenceLife66 :: Int
  , willing_ProfOfficeHours67 :: Int
  , willing_Tutor68 :: Int
  , willing_WritingCenter69 :: Int
  , willing_SkillsEnhanceWrkshp70 :: Int
  , willing_CareerCenter71 :: Int
  , willing_Librarian72 :: Int
  , likely_ProfOfficeHours73 :: Int
  , likely_Tutor74 :: Int
  , likely_WritingCenter75 :: Int
  , likely_SkillsEnhanceWrkshp76 :: Int
  , likely_CareerCenter77 :: Int
  , likely_Librarian78 :: Int
  , distanceFromCampus79 :: Int
  , timeToCommute80 :: Int
  , prevYearActivities81 :: Int
  , freq_GenThesis82 :: Int
  , freq_ReviseDraft83 :: Int
  , freq_UseCitation84 :: Int
  , freq_WorkWithPeopleDiffFromMe85 :: Int
  , freq_TeamProject86 :: Int
  , freq_UseGraphics87 :: Int
  , freq_OralPresentation88 :: Int
  , freq_QuantReasoning89 :: Int
  , freq_IntegrateIdeas90 :: Int
  , freq_DiscussSocialProblems91 :: Int
  , freq_UseLibrary92 :: Int
  , rate_GenThesis93 :: Int
  , rate_ReviseDraft94 :: Int
  , rate_UseCitation95 :: Int
  , rate_WorkWithPeopleDiffFromMe96 :: Int
  , rate_TeamProject97 :: Int
  , rate_UseGraphics98  :: Int
  , rate_OralPresentation99 :: Int
  , rate_QuantReasoning100 :: Int
  , rate_IngegrateIdeas101 :: Int
  , rate_DiscussSocialProblems102 :: Int
  , rate_UseLibrary103 :: Int
  , communityResponToServe104 :: Int
  , communityMakeADifference105 :: Int
  , communityDefinePersonalStrength106 :: Int
  , communityEnhancedLeadership107 :: Int
  , communityServiceInLastYear108 :: Int
  , age109 :: Int
  , gender110:: Int
  , highestEducationMother111 :: Int
  , highestEducationFather112 :: Int
  , support_Parents113 :: Int
  , support_SignificantOther114 :: Int
  , support_Employer115 :: Int
  , support_Friends116 :: Int
  , support_ExtendedFamily117 :: Int
  , areYouRegisteredToVote118 :: Int
  , didYouVoteInTheLastElection119 :: Int
  , doYouIntendToVoteInTheNext120 :: Int
  } 

field :: Int -> Answer -> Int
field 1 =   edPlan1
field 2 =   highDegree2
field 3 =   howLikelyAreYouToCompleteBachelorsDegreeAtPsu3
field 4 =   why_AdvanceJob4
field 5 =   why_BetterJob5
field 6 =   why_BetterInformed6
field 7 =   why_GeneralEd7
field 8 =   why_PrepForGrad8
field 9 =   why_Parents9
field 10 =   why_MeetPeople10
field 11 =   why_EarnMoney11
field 12 =   psu_Cost12
field 13 =   psu_Location13
field 14 =   psu_Majors14
field 15 =   psu_AdmRequirements15
field 16 =   psu_AbleToLiveAtHome16
field 17 =   psu_Reputation17
field 18 =   psu_Athletics18
field 19 =   psu_Academics19
field 20 =   psu_UnivStudies20
field 21 =   psu_CommBasedLearn21
field 22 =   psu_FamilyRecomend22
field 23 =   psu_FriendsAttend23
field 24 =   psu_DiverseStudentBody24
field 25 =   psu_FinancialAid25
field 26 =   advising_NewStudOrient26
field 27 =   advising_UASCAppointment27
field 28 =   advising_UASCworkshop28
field 29 =   advising_OtherPsuAdvisor29
field 30 =   advising_Other30
field 31 =   understandGradRequirements31
field 32 =   frinqRegThemeInterest32
field 33 =   frinqRegTime33
field 34 =   topConcernBeginYearAtPsu34
field 35 =   concernClass_NotSmartEnough35
field 36 =   concernClass_MightFail36
field 37 =   concernClass_KeepUpWithWork37
field 38 =   concernClass_MakeFriends38
field 39 =   concernClass_TooManyCredits39
field 40 =   concernClass_Other40
field 41 =   error "The Major component is a String (not an Int), cannot use the field function!"
field 42 =   expectedCreditsPerTerm42
field 43 =   workHoursThisYear43
field 44 =   workLocation44
field 45 =   computer_Home45
field 46 =   computer_Work46
field 47 =   computer_PsuLab47
field 48 =   computer_PublicLibrary48
field 49 =   computer_FriendOrFamily49
field 50 =   computer_NoAccess50
field 51 =   computer_Other51
field 52 =   dependents52
field 53 =   financialAid53
field 54 =   transportation_Walk54
field 55 =   transportation_Drive55
field 56 =   transportation_Carpool56
field 57 =   transportation_Bike57
field 58 =   transportation_PublicTrans58
field 59 =   housingType59
field 60 =   part_StudentClub60
field 61 =   part_VolInComm61
field 62 =   part_StudentGovt62
field 63 =   part_GreekLife63
field 64 =   part_Intramural64
field 65 =   part_OutdoorProgram65
field 66 =   part_ResidenceLife66
field 67 =   willing_ProfOfficeHours67
field 68 =   willing_Tutor68
field 69 =   willing_WritingCenter69
field 70 =   willing_SkillsEnhanceWrkshp70
field 71 =   willing_CareerCenter71
field 72 =   willing_Librarian72
field 73 =   likely_ProfOfficeHours73
field 74 =   likely_Tutor74
field 75 =   likely_WritingCenter75
field 76 =   likely_SkillsEnhanceWrkshp76
field 77 =   likely_CareerCenter77
field 78 =   likely_Librarian78
field 79 =   distanceFromCampus79
field 80 =   timeToCommute80
field 81 =   prevYearActivities81
field 82 =   freq_GenThesis82
field 83 =   freq_ReviseDraft83
field 84 =   freq_UseCitation84
field 85 =   freq_WorkWithPeopleDiffFromMe85
field 86 =   freq_TeamProject86
field 87 =   freq_UseGraphics87
field 88 =   freq_OralPresentation88
field 89 =   freq_QuantReasoning89
field 90 =   freq_IntegrateIdeas90
field 91 =   freq_DiscussSocialProblems91
field 92 =   freq_UseLibrary92
field 93 =   rate_GenThesis93
field 94 =   rate_ReviseDraft94
field 95 =   rate_UseCitation95
field 96 =   rate_WorkWithPeopleDiffFromMe96
field 97 =   rate_TeamProject97
field 98 =   rate_UseGraphics98 
field 99 =   rate_OralPresentation99
field 100 =   rate_QuantReasoning100
field 101 =   rate_IngegrateIdeas101
field 102 =   rate_DiscussSocialProblems102
field 103 =   rate_UseLibrary103
field 104 =   communityResponToServe104
field 105 =   communityMakeADifference105
field 106 =   communityDefinePersonalStrength106
field 107 =   communityEnhancedLeadership107
field 108 =   communityServiceInLastYear108
field 109 =   age109
field 110 =   gender110
field 111 =   highestEducationMother111
field 112 =   highestEducationFather112
field 113 =   support_Parents113
field 114 =   support_SignificantOther114
field 115 =   support_Employer115
field 116 =   support_Friends116
field 117 =   support_ExtendedFamily117
field 118 =   areYouRegisteredToVote118
field 119 =   didYouVoteInTheLastElection119
field 120 =   doYouIntendToVoteInTheNext120



name :: Int -> String
name 1 =   "edPlan1"
name 2 =   "highDegree2"
name 3 =   "howLikelyAreYouToCompleteBachelorsDegreeAtPsu3"
name 4 =   "why_AdvanceJob4"
name 5 =   "why_BetterJob5"
name 6 =   "why_BetterInformed6"
name 7 =   "why_GeneralEd7"
name 8 =   "why_PrepForGrad8"
name 9 =   "why_Parents9"
name 10 =   "why_MeetPeople10"
name 11 =   "why_EarnMoney11"
name 12 =   "psu_Cost12"
name 13 =   "psu_Location13"
name 14 =   "psu_Majors14"
name 15 =   "psu_AdmRequirements15"
name 16 =   "psu_AbleToLiveAtHome16"
name 17 =   "psu_Reputation17"
name 18 =   "psu_Athletics18"
name 19 =   "psu_Academics19"
name 20 =   "psu_UnivStudies20"
name 21 =   "psu_CommBasedLearn21"
name 22 =   "psu_FamilyRecomend22"
name 23 =   "psu_FriendsAttend23"
name 24 =   "psu_DiverseStudentBody24"
name 25 =   "psu_FinancialAid25"
name 26 =   "advising_NewStudOrient26"
name 27 =   "advising_UASCAppointment27"
name 28 =   "advising_UASCworkshop28"
name 29 =   "advising_OtherPsuAdvisor29"
name 30 =   "advising_Other30"
name 31 =   "understandGradRequirements31"
name 32 =   "frinqRegThemeInterest32"
name 33 =   "frinqRegTime33"
name 34 =   "topConcernBeginYearAtPsu34"
name 35 =   "concernClass_NotSmartEnough35"
name 36 =   "concernClass_MightFail36"
name 37 =   "concernClass_KeepUpWithWork37"
name 38 =   "concernClass_MakeFriends38"
name 39 =   "concernClass_TooManyCredits39"
name 40 =   "concernClass_Other40"
name 41 =   "major41"
name 42 =   "expectedCreditsPerTerm42"
name 43 =   "workHoursThisYear43"
name 44 =   "workLocation44"
name 45 =   "computer_Home45"
name 46 =   "computer_Work46"
name 47 =   "computer_PsuLab47"
name 48 =   "computer_PublicLibrary48"
name 49 =   "computer_FriendOrFamily49"
name 50 =   "computer_NoAccess50"
name 51 =   "computer_Other51"
name 52 =   "dependents52"
name 53 =   "financialAid53"
name 54 =   "transportation_Walk54"
name 55 =   "transportation_Drive55"
name 56 =   "transportation_Carpool56"
name 57 =   "transportation_Bike57"
name 58 =   "transportation_PublicTrans58"
name 59 =   "housingType59"
name 60 =   "part_StudentClub60"
name 61 =   "part_VolInComm61"
name 62 =   "part_StudentGovt62"
name 63 =   "part_GreekLife63"
name 64 =   "part_Intramural64"
name 65 =   "part_OutdoorProgram65"
name 66 =   "part_ResidenceLife66"
name 67 =   "willing_ProfOfficeHours67"
name 68 =   "willing_Tutor68"
name 69 =   "willing_WritingCenter69"
name 70 =   "willing_SkillsEnhanceWrkshp70"
name 71 =   "willing_CareerCenter71"
name 72 =   "willing_Librarian72"
name 73 =   "likely_ProfOfficeHours73"
name 74 =   "likely_Tutor74"
name 75 =   "likely_WritingCenter75"
name 76 =   "likely_SkillsEnhanceWrkshp76"
name 77 =   "likely_CareerCenter77"
name 78 =   "likely_Librarian78"
name 79 =   "distanceFromCampus79"
name 80 =   "timeToCommute80"
name 81 =   "prevYearActivities81"
name 82 =   "freq_GenThesis82"
name 83 =   "freq_ReviseDraft83"
name 84 =   "freq_UseCitation84"
name 85 =   "freq_WorkWithPeopleDiffFromMe85"
name 86 =   "freq_TeamProject86"
name 87 =   "freq_UseGraphics87"
name 88 =   "freq_OralPresentation88"
name 89 =   "freq_QuantReasoning89"
name 90 =   "freq_IntegrateIdeas90"
name 91 =   "freq_DiscussSocialProblems91"
name 92 =   "freq_UseLibrary92"
name 93 =   "rate_GenThesis93"
name 94 =   "rate_ReviseDraft94"
name 95 =   "rate_UseCitation95"
name 96 =   "rate_WorkWithPeopleDiffFromMe96"
name 97 =   "rate_TeamProject97"
name 98 =   "rate_UseGraphics98 "
name 99 =   "rate_OralPresentation99"
name 100 =   "rate_QuantReasoning100"
name 101 =   "rate_IngegrateIdeas101"
name 102 =   "rate_DiscussSocialProblems102"
name 103 =   "rate_UseLibrary103"
name 104 =   "communityResponToServe104"
name 105 =   "communityMakeADifference105"
name 106 =   "communityDefinePersonalStrength106"
name 107 =   "communityEnhancedLeadership107"
name 108 =   "communityServiceInLastYear108"
name 109 =   "age109"
name 110 =   "gender110"
name 111 =   "highestEducationMother111"
name 112 =   "highestEducationFather112"
name 113 =   "support_Parents113"
name 114 =   "support_SignificantOther114"
name 115 =   "support_Employer115"
name 116 =   "support_Friends116"
name 117 =   "support_ExtendedFamily117"
name 118 =   "areYouRegisteredToVote118"
name 119 =   "didYouVoteInTheLastElection119"
name 120 =   "doYouIntendToVoteInTheNext120"

instance Show Answer where
  show (Ans x1   x2   x3   x4   x5   x6   x7   x8   x9   x10
            x11  x12  x13  x14  x15  x16  x17  x18  x19  x20
            x21  x22  x23  x24  x25  x26  x27  x28  x29  x30
            x31  x32  x33  x34  x35  x36  x37  x38  x39  x40
            x41  x42  x43  x44  x45  x46  x47  x48  x49  x50
            x51  x52  x53  x54  x55  x56  x57  x58  x59  x60
            x61  x62  x63  x64  x65  x66  x67  x68  x69  x70
            x71  x72  x73  x74  x75  x76  x77  x78  x79  x80
            x81  x82  x83  x84  x85  x86  x87  x88  x89  x90
            x91  x92  x93  x94  x95  x96  x97  x98  x99  x100
            x101 x102 x103 x104 x105 x106 x107 x108 x109 x110
            x111 x112 x113 x114 x115 x116 x117 x118 x119 x120) = "   (Ans "++ 
           f [ x1,  x2,  x3,  x4,  x5,  x6,  x7,  x8,  x9,  x10
             , x11, x12, x13, x14, x15, x16, x17, x18, x19, x20
             , x21, x22, x23, x24, x25, x26, x27, x28, x29, x30
             , x31, x32, x33, x34, x35, x36, x37, x38, x39, x40]
            ++ " \""++ x41 ++ "\" " ++
            f [x42, x43, x44, x45, x46, x47, x48, x49, x50
             , x51, x52, x53, x54, x55, x56, x57, x58, x59, x60
             , x61, x62, x63, x64, x65, x66, x67, x68, x69, x70
             , x71, x72, x73, x74, x75, x76, x77, x78, x79, x80
             , x81, x82, x83, x84, x85, x86, x87, x88, x89, x90
             , x91, x92, x93, x94, x95, x96, x97, x98, x99, x100
             , x101, x102, x103, x104, x105, x106, x107, x108, x109, x110
             , x111, x112, x113, x114, x115, x116, x117, x118, x119, x120] ++ ")"
    where f [x] = show x
          f (x:xs) = show x ++ " " ++ f xs
          f [] = ""
        
        

priorLearnData =
  [(Ans 6 5 4 1 3 4 4 2 3 4 3 1 4 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 0 1 7 1 0 0 0 0 0 "Undecided" 12 3 2 1 0 0 0 0 0 0 1 1 3 1 1 1 1 2 1 0 0 0 0 0 0 3 3 4 3 3 4 3 3 2 2 2 4 0 5 6 5 5 5 5 5 5 5 5 5 5 0 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 0 101 2 5 4 3 3 4 4 3 3 3 3)

  ,(Ans 6 6 2 1 4 4 2 1 2 4 4 4 4 4 101 101 101 101 101 101 101 101 3 3 101 1 0 1 1 0 3 1 0 7 0 1 1 0 0 0 "Undecided" 13 2 2 1 0 0 0 0 0 0 101 6 101 101 101 101 2 1 1 1 0 0 0 0 0 3 101 4 101 4 101 101 101 101 101 101 101 101 50 6 101 101 1 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 4 5 4 101 1 101 2 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 4 4 4 4 4 3 4 4 1 3 4 2 4 1 3 1 3 1 3 2 2 3 1 1 0 0 0 0 5 1 1 4 0 1 1 0 0 0 "Philosophy" 12 2 101 1 0 1 0 0 0 0 1 3 3 101 101 101 3 2 1 0 0 0 0 1 0 3 3 3 3 4 3 3 3 3 3 4 3 0 0 4 4 3 2 1 5 5 5 2 5 1 1 3 3 3 3 101 4 4 4 3 3 3 3 3 3 3 1 101 2 4 5 3 3 4 3 3 2 2 2)

  ,(Ans 6 6 4 3 4 4 4 2 2 4 4 4 4 4 2 1 2 2 3 2 3 3 3 3 1 0 1 0 0 0 4 0 1 7 0 0 0 0 0 1 "Communication Studies" 14 6 2 1 1 1 0 0 0 0 1 1 1 3 1 1 1 2 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 20 25 5 5 4 4 0 4 5 5 5 5 0 2 3 3 3 3 3 3 3 3 3 3 3 4 3 99 99 0 101 2 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 4 4 4 4 2 4 3 4 4 4 4 1 2 1 2 4 2 2 3 1 4 0 1 0 0 0 3 1 0 7 1 0 0 0 0 0 "Social Work" 12 5 2 1 0 0 0 0 0 0 3 3 2 2 1 1 1 2 0 1 0 0 0 0 0 4 4 4 3 3 4 101 101 101 101 101 101 6 30 6 1 2 2 0 3 1 1 0 3 1 1 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 1 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 3 6 3 3 3 4 4 2 1 3 3 3 101 4 3 1 3 1 3 1 3 1 4 2 4 0 0 0 1 0 4 1 1 1 0 0 1 0 0 0 "Film" 13 3 1 1 0 1 1 1 0 0 1 2 1 1 1 1 3 3 0 0 0 0 0 0 0 4 4 3 3 101 3 3 1 2 3 3 3 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 4 4 1 4 4 3 3 3 4 4 4 3 4 3 3 2 3 2 1 2 1 3 1 2 1 0 0 0 0 2 0 1 2 0 0 1 0 0 0 "Political Science" 15 3 2 1 0 1 1 1 0 0 3 2 101 2 101 101 2 1 1 0 1 0 0 0 0 4 3 2 2 4 4 3 2 2 1 3 4 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 4 4 4 4 4 1 4 1 3 4 3 3 1 4 1 4 4 4 1 4 4 4 0 1 0 0 0 5 1 1 2 0 0 0 0 0 0 "Accounting" 17 3 2 0 0 1 0 0 0 0 1 6 101 101 101 101 3 101 0 0 0 0 1 0 0 101 101 101 101 101 101 4 101 101 101 101 101 5 60 5 5 5 5 5 5 5 5 101 5 5 5 5 5 5 5 5 5 5 101 5 5 5 4 5 4 5 1 101 1 5 5 3 3 2 3 3 0 0 3)

  ,(Ans 6 6 4 3 4 4 4 4 3 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 0 1 0 3 1 1 2 0 0 1 0 1 0 "Accounting" 16 1 101 0 0 0 0 0 0 0 101 101 101 101 101 101 101 101 0 0 0 0 0 0 0 101 101 101 101 101 101 101 101 101 101 101 101 2 10 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 1 2 101 101 101 101 101 101 101 101 101)

  ,(Ans 3 3 2 1 4 4 1 3 2 2 1 4 2 4 1 4 1 1 1 1 1 1 1 1 1 0 1 0 0 0 5 1 1 99 0 0 0 0 0 1 "Political Science" 16 1 101 1 0 1 0 0 0 0 1 5 3 3 101 101 3 1 0 0 0 0 0 0 0 4 2 3 2 4 4 1 1 1 1 1 1 15 55 0 5 5 5 0 5 5 3 5 4 5 5 5 5 5 4 4 5 3 5 5 5 5 4 4 4 5 1 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 3 1 1 4 4 2 2 2 1 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1 0 0 0 0 4 1 1 99 1 1 1 1 0 1 "Music" 16 1 101 1 0 1 1 1 0 0 1 1 1 2 1 1 3 2 1 1 0 0 0 0 0 3 2 4 3 3 4 3 2 4 2 3 4 15 75 6 5 5 5 5 5 5 5 3 5 5 5 4 4 3 5 5 5 4 4 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 3 4 4 4 4 2 4 4 3 4 4 3 3 4 4 1 4 4 4 3 1 2 2 1 0 0 0 1 3 0 1 0 0 0 0 0 0 0 "Undecided" 13 1 101 1 0 1 1 0 0 0 1 6 3 101 101 101 3 1 0 0 0 0 0 1 0 4 4 3 3 3 4 2 2 2 2 1 2 15 60 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 3 3 3 2 3 2 2 2 4 4 3 3 1 101 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 1 4 4 4 3 1 4 2 1 4 3 3 4 2 1 3 1 1 1 1 1 3 0 1 0 0 0 3 1 1 0 0 0 0 0 0 1 "English" 12 2 1 1 0 1 1 1 0 1 1 6 3 1 1 1 2 2 0 0 0 0 0 0 0 4 4 4 4 3 4 4 3 4 3 3 4 1 15 99 5 5 3 2 5 3 3 5 5 2 5 1 1 1 2 1 5 4 4 5 4 3 5 5 5 5 1 101 1 2 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 1 1 1 1 4 1 2 2 4 3 1 2 1 3 1 1 1 1 1 4 0 0 0 0 1 2 1 0 7 0 0 0 0 0 1 "Art" 20 1 101 1 0 1 1 0 0 0 3 2 1 3 1 1 2 2 1 0 0 0 0 0 0 4 4 4 4 4 4 1 1 1 1 1 1 20 60 5 0 0 0 0 0 0 0 0 0 0 0 2 2 2 5 5 5 5 3 3 5 3 1 1 1 1 1 101 1 2 2 1 1 1 1 1 3 3 3)

  ,(Ans 6 6 4 1 4 1 1 1 1 4 1 1 4 4 3 4 3 1 3 1 1 1 1 1 4 0 0 0 1 0 5 1 1 0 0 0 0 0 0 1 "Civil Engineering" 12 1 101 0 0 1 1 0 0 0 1 5 101 101 101 101 3 2 0 0 0 0 0 0 0 4 1 1 1 1 4 2 1 1 1 1 4 4 45 99 0 2 0 0 0 0 0 0 2 0 0 4 4 4 2 2 2 2 1 2 2 2 2 3 2 2 0 101 1 99 99 4 4 4 3 4 0 0 0)

  ,(Ans 99 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 1 0 0 1 0 4 1 1 7 1 0 1 0 0 0 "Accounting" 12 2 1 1 0 0 0 0 0 0 1 6 3 3 101 101 3 2 0 0 1 0 0 1 0 1 1 2 1 1 1 1 1 2 1 1 1 20 55 99 0 0 0 0 0 0 0 0 0 0 0 3 4 5 3 4 3 5 3 4 2 101 4 5 5 5 0 101 1 3 5 3 3 101 3 3 1 1 1)

  ,(Ans 6 3 4 2 4 3 3 4 1 3 2 3 3 4 3 3 3 1 4 2 3 2 3 3 3 1 0 0 0 0 4 0 0 0 0 0 0 0 0 1 "Civil Engineering" 14 3 2 1 0 1 0 0 0 0 2 6 3 101 101 101 3 2 1 1 0 0 0 0 0 3 4 101 4 4 3 3 3 3 3 3 2 10 100 5 2 2 1 3 3 2 3 1 1 1 1 2 2 2 2 2 2 3 2 3 2 2 4 4 4 4 1 101 2 3 2 2 4 4 3 2 3 3 3)

  ,(Ans 6 4 4 2 2 4 4 2 1 1 3 4 4 2 1 4 1 1 1 1 2 1 1 1 1 0 0 0 0 1 3 1 1 7 0 0 0 0 0 1 "English" 13 5 3 1 1 1 1 0 0 0 1 6 2 1 1 2 3 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 2 4 1 4 2 5 30 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 5 5 5 5 5 5 5 3 4 5 1 101 1 3 3 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 4 1 1 4 1 3 3 1 1 1 1 1 1 4 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Art" 13 2 2 1 0 0 0 0 0 0 1 1 1 3 101 101 101 2 0 0 0 0 0 0 0 4 4 4 4 1 4 101 101 101 101 101 101 5 15 5 0 0 0 0 0 0 0 0 0 0 0 1 1 1 4 4 1 1 4 3 1 2 4 4 4 4 0 101 1 3 5 3 3 4 4 4 0 0 0)

  ,(Ans 6 3 4 1 3 4 4 4 1 1 1 3 4 3 4 4 3 1 2 1 3 1 1 2 1 1 0 0 0 0 5 1 0 3 0 0 0 0 0 0 "Science" 15 1 101 1 0 1 1 0 0 0 2 2 1 2 1 3 3 2 0 1 0 0 0 0 0 4 4 4 4 3 3 4 4 4 4 2 1 5 45 5 0 0 0 0 5 5 5 5 5 5 5 1 1 1 5 5 5 4 4 4 3 4 5 5 3 4 1 101 1 2 2 4 3 4 4 4 101 1 1)

  ,(Ans 6 4 4 1 4 4 4 4 1 3 2 4 4 4 4 4 4 1 4 2 4 4 4 4 4 1 0 0 0 0 4 0 0 7 0 0 0 0 0 0 "Philosophy" 14 3 2 1 1 0 0 0 0 0 3 6 3 1 1 2 1 2 1 0 0 0 0 1 0 4 4 4 4 4 4 4 3 4 2 2 4 0 5 5 0 5 0 5 5 0 5 0 0 0 0 1 1 1 5 5 1 2 2 4 3 1 5 5 3 4 0 101 1 2 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 2 1 3 4 4 4 2 4 1 1 1 3 3 3 1 1 4 4 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 6 2 1 1 1 3 2 1 1 0 0 0 1 0 1 1 1 1 1 1 1 1 1 1 1 1 5 15 0 0 0 0 0 5 0 0 0 0 0 0 1 1 1 1 5 1 1 1 1 1 1 5 5 3 3 0 101 1 3 2 3 3 4 3 3 1 1 1)

  ,(Ans 6 6 4 2 4 3 3 2 2 4 2 3 4 3 2 4 3 1 3 2 2 2 2 2 4 1 0 0 0 0 4 0 1 5 0 0 0 0 0 0 "Biology" 12 5 2 1 0 1 1 0 0 0 3 6 2 2 1 3 1 2 0 0 0 0 0 0 0 4 4 4 3 2 4 4 4 4 3 2 4 3 20 5 0 5 5 0 5 0 5 0 2 0 0 2 2 2 4 4 3 5 5 4 3 4 4 4 2 4 1 101 1 4 4 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 1 1 1 3 3 4 1 4 2 1 3 1 3 1 1 3 2 0 0 0 0 1 4 1 1 5 0 0 0 0 0 1 "Architecture" 14 6 2 1 1 1 1 1 0 0 3 6 1 3 2 2 2 2 0 0 0 0 0 0 0 4 3 2 1 1 3 4 1 1 1 1 4 15 25 99 2 5 5 5 5 4 5 3 5 5 5 3 5 5 3 3 5 5 4 5 5 5 3 5 1 99 0 101 1 1 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 1 1 4 2 2 4 2 1 1 1 3 1 1 4 2 0 1 0 0 0 4 0 0 5 0 0 0 0 0 1 "Psychology" 11 2 3 1 0 1 1 0 0 0 2 2 101 101 101 101 3 2 0 0 0 0 0 0 0 2 3 1 4 4 4 4 4 4 1 4 4 6 30 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 2 4 1 101 101 101 101 101 101 101 101)

  ,(Ans 6 4 4 2 3 4 4 3 1 3 3 3 4 4 3 2 3 1 2 1 1 1 1 3 4 1 0 1 0 0 5 1 1 0 0 0 0 0 0 1 "Psychology" 13 2 1 1 1 1 1 0 0 0 1 6 1 1 1 1 3 2 0 1 0 0 0 0 0 4 4 4 3 3 3 4 3 4 3 2 2 20 60 2 4 4 4 0 1 0 0 2 4 0 5 4 5 5 2 3 4 1 5 5 2 5 4 4 4 3 1 101 2 3 2 3 2 4 3 4 1 1 0)

  ,(Ans 6 5 4 1 4 4 4 2 1 3 1 4 4 3 1 4 2 1 3 2 2 1 3 2 4 0 0 0 1 0 4 1 0 99 0 1 1 0 1 0 "Civil Engineering" 15 1 101 1 0 1 1 1 0 1 2 6 2 1 1 3 3 2 0 0 0 0 0 0 0 4 3 3 3 1 3 4 3 3 2 1 3 6 40 2 0 0 0 4 5 0 1 5 5 3 0 2 2 2 1 2 2 1 4 2 2 1 3 3 3 4 1 101 1 4 3 3 3 4 3 2 1 1 1)

  ,(Ans 6 6 4 1 4 4 4 4 1 4 2 2 2 4 3 1 4 1 4 4 4 1 1 4 4 1 0 0 0 0 4 0 0 7 0 0 1 0 0 0 "Environmental Sciences and Resources" 17 1 101 1 0 1 0 1 0 0 3 6 101 2 2 2 3 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 20 60 99 0 0 0 5 5 0 0 5 5 5 5 4 4 5 4 5 3 5 5 5 5 4 101 5 5 5 1 101 1 99 2 1 3 4 3 4 0 0 0)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 3 3 3 3 1 1 1 1 3 1 1 1 1 1 3 0 0 0 1 0 3 0 0 7 0 0 0 0 0 1 "" 16 6 2 1 1 1 0 0 0 0 1 2 101 2 101 101 2 2 0 0 0 0 0 0 0 4 4 4 3 3 3 2 2 2 2 3 1 15 35 2 3 5 5 5 5 5 5 5 5 5 5 2 3 3 4 4 3 3 2 3 3 3 4 4 4 4 1 101 2 3 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 3 1 4 2 1 3 1 4 2 2 1 3 3 1 1 1 2 1 2 2 4 2 4 1 0 0 0 0 2 0 0 7 0 0 0 0 0 0 "Computer Science" 14 4 3 1 0 1 0 1 0 0 1 6 1 2 3 1 2 2 1 0 0 0 0 0 0 4 2 2 2 2 2 4 3 3 3 4 4 15 30 99 0 2 0 0 5 3 3 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 3 4 3 5 1 101 1 99 3 3 3 3 3 3 1 0 0)

  ,(Ans 6 5 4 1 4 4 4 3 1 3 3 4 3 4 3 4 4 1 4 3 4 4 1 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Environmental Studies" 12 4 2 1 1 1 1 0 0 0 1 2 2 101 101 3 2 2 0 0 0 0 0 1 0 4 4 4 4 3 4 3 3 4 3 3 3 5 30 5 0 1 0 5 0 0 0 0 0 3 1 2 3 2 5 5 2 2 3 3 3 3 4 4 4 3 1 101 2 5 3 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 3 4 4 4 4 1 3 3 4 4 3 3 4 4 1 4 2 3 1 3 4 3 1 0 0 0 0 4 1 1 0 0 1 1 0 0 0 "Undecided" 12 3 2 1 1 1 1 0 0 0 1 6 2 1 2 3 3 2 1 0 1 0 0 0 0 4 3 4 4 4 4 3 3 4 3 4 4 6 20 5 0 5 0 5 0 5 5 0 5 5 0 1 2 2 4 3 2 2 2 4 4 3 5 5 3 3 0 101 2 4 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 1 2 3 3 2 4 3 2 1 4 2 4 3 3 2 1 2 4 1 0 1 0 0 4 0 1 0 0 0 1 0 0 0 "Business Administration" 13 2 2 1 0 1 0 1 0 0 1 6 1 3 1 1 2 2 0 1 0 0 0 0 0 4 4 4 3 4 4 3 3 4 3 2 2 8 15 5 0 0 0 0 0 0 0 0 0 2 0 2 2 2 4 3 2 2 2 3 3 3 4 3 99 99 0 101 1 3 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 4 4 4 4 4 4 4 4 1 4 4 4 4 4 4 3 0 0 0 1 0 5 0 1 0 0 0 0 0 0 1 "Social Science" 13 1 99 0 0 1 1 1 0 0 2 3 1 3 2 1 2 2 0 1 0 1 0 0 0 101 101 101 4 101 101 4 4 4 3 4 4 9 20 5 5 4 5 5 5 5 3 1 5 5 5 3 3 3 5 5 3 5 2 5 5 5 5 5 5 5 1 101 2 2 1 3 3 4 3 3 1 1 1)

  ,(Ans 6 6 4 4 4 4 4 3 3 4 3 4 4 4 4 4 4 2 4 4 4 2 2 4 4 1 0 0 0 0 2 1 1 7 0 0 0 0 0 0 "Mechanical Engineering" 9 5 99 0 0 1 1 0 0 0 1 2 2 101 101 2 3 2 1 1 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 3 45 5 1 2 0 2 5 5 5 0 5 5 0 1 1 1 3 3 2 2 2 3 4 2 5 5 4 5 1 101 1 2 4 2 3 4 4 1 3 3 3)

  ,(Ans 6 4 4 1 4 3 3 4 1 4 3 3 3 3 1 4 3 1 3 1 2 1 1 2 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Political Science" 15 5 2 1 0 0 0 0 0 0 1 2 101 101 101 101 3 2 0 0 1 0 0 0 0 4 3 3 3 3 4 4 3 3 2 2 4 10 45 5 0 0 0 3 5 0 5 0 0 3 0 2 2 3 5 5 2 5 5 3 5 3 5 5 5 5 1 101 1 3 3 3 3 3 3 2 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 4 2 4 3 1 2 4 2 4 3 1 3 1 2 1 4 4 4 0 0 0 1 0 5 1 1 0 0 0 0 0 0 1 "Foreign Languages" 12 2 2 1 1 1 1 1 0 0 1 6 1 2 101 101 3 2 0 0 0 0 0 0 0 4 4 3 2 4 3 3 3 3 2 3 2 5 30 5 2 4 2 0 1 1 2 3 2 0 1 3 4 4 4 4 3 4 4 4 4 4 4 4 3 3 1 101 2 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 3 6 4 1 4 3 3 3 1 4 3 4 4 4 4 4 1 1 1 1 1 1 4 2 4 1 1 0 1 0 4 1 1 7 0 0 0 0 0 1 "Foreign Languages" 13 6 2 1 0 1 1 1 0 0 1 6 3 1 1 1 2 2 0 0 0 0 0 0 0 4 1 1 1 1 4 4 2 2 2 2 3 2 30 5 5 5 5 5 5 1 5 5 5 5 5 5 5 4 3 3 2 3 4 5 5 5 2 3 1 1 0 101 1 1 1 4 3 3 3 4 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 1 4 4 4 2 4 3 3 4 1 4 3 2 1 4 4 4 1 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "Psychology" 12 5 2 1 1 1 0 0 0 0 1 2 1 1 1 2 3 2 1 0 0 0 0 0 0 4 3 3 3 3 2 4 3 3 3 3 1 10 30 5 4 5 4 2 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 5 3 3 1 101 1 99 2 3 2 3 4 2 3 3 3)

  ,(Ans 6 3 4 4 4 4 4 3 2 4 3 2 3 3 2 1 3 1 2 2 3 3 3 2 1 0 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Accounting" 15 4 2 1 0 1 0 1 0 0 1 3 1 3 2 1 1 2 0 0 0 0 0 0 0 3 3 4 1 1 1 101 101 101 101 101 101 12 20 5 0 0 5 0 1 0 0 0 1 0 1 1 1 2 2 2 2 1 2 2 2 2 4 4 3 3 0 101 1 1 1 3 3 4 2 3 0 0 0)

  ,(Ans 6 2 3 1 4 4 4 4 3 4 2 1 2 1 1 1 3 2 3 1 3 1 4 3 3 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Environmental Sciences and Resources" 16 3 2 1 1 1 0 0 0 0 1 6 3 1 2 3 3 2 0 1 0 0 1 0 0 4 4 3 3 3 4 4 4 3 3 3 4 3 45 5 0 2 0 0 5 0 5 0 0 2 0 2 3 3 4 5 2 3 3 5 4 5 5 5 2 4 1 101 1 2 99 1 3 2 1 1 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 2 1 2 4 4 4 4 4 4 3 1 3 4 4 3 1 2 4 1 1 0 0 0 2 1 1 0 1 1 0 0 0 0 "Administration of Justice" 12 2 2 1 0 0 0 0 0 0 1 3 3 2 1 1 3 2 0 1 0 0 0 1 0 101 101 101 101 3 4 4 4 4 4 101 101 3 15 5 2 5 5 2 3 2 5 0 5 2 4 2 2 2 4 4 2 5 2 2 3 3 5 5 4 4 1 101 2 3 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 3 4 4 2 4 2 4 2 4 2 3 3 4 3 4 4 3 4 4 4 2 3 2 4 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Political Science" 10 6 2 1 0 0 0 0 0 0 1 3 1 3 1 1 1 1 0 1 0 0 0 0 0 4 4 4 4 4 4 3 2 3 3 2 2 10 25 5 0 1 0 2 2 3 2 1 2 2 2 1 2 3 4 4 4 3 2 2 5 5 4 3 4 4 0 25 2 2 101 3 4 4 3 3 3 0 3)

  ,(Ans 6 2 4 1 3 4 4 4 1 2 1 1 2 2 2 1 2 1 2 1 2 1 1 1 4 1 0 0 0 0 4 0 1 7 0 0 0 0 0 0 "Psychology" 12 6 2 1 0 1 1 0 0 0 3 6 1 1 1 3 2 2 0 0 0 0 0 0 0 4 3 4 4 3 1 4 1 2 1 1 1 6 30 5 2 2 2 0 1 5 0 2 2 0 3 2 3 3 4 3 4 3 3 3 2 4 5 5 3 3 1 25 1 3 5 4 3 3 3 3 1 1 1)

  ,(Ans 6 4 4 3 4 4 4 4 1 3 3 3 3 4 3 1 3 1 3 1 4 1 3 4 3 1 0 0 0 1 2 1 0 7 0 0 0 0 0 1 "Biology" 13 5 2 1 1 1 1 1 0 0 1 2 2 1 2 2 3 2 1 1 0 0 0 1 0 4 4 4 4 4 4 4 4 4 4 4 4 2 20 5 0 0 0 5 5 5 5 2 3 1 0 4 4 3 4 4 1 5 3 3 3 3 5 5 5 5 0 25 2 2 1 1 4 3 3 3 1 0 1)

  ,(Ans 6 5 4 1 4 3 3 3 2 3 1 2 2 2 2 4 3 1 3 1 3 1 3 3 3 1 0 0 0 0 2 1 0 7 0 0 0 0 0 1 "History" 13 5 2 1 0 0 0 0 0 0 3 6 101 101 3 101 2 2 0 0 0 0 0 0 0 3 2 3 2 3 4 3 2 3 2 3 4 6 20 5 0 0 0 0 5 0 3 0 0 0 0 4 4 2 4 3 3 2 3 4 4 4 4 4 3 3 0 25 1 2 5 3 3 3 3 3 1 1 1)

  ,(Ans 6 4 4 3 4 4 4 4 1 2 3 101 4 4 3 1 2 1 2 1 2 1 1 3 4 0 0 0 0 1 4 1 1 7 0 0 0 0 0 0 "Foreign Languages" 12 4 2 1 0 1 1 0 0 0 1 6 2 1 1 2 3 2 1 0 0 0 0 0 0 4 2 3 1 3 3 4 1 3 3 2 4 4 40 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 2 3 1 25 2 3 2 2 3 4 3 2 1 1 1)

  ,(Ans 6 4 4 2 3 4 4 4 1 3 2 2 4 2 1 4 3 3 2 2 2 1 1 1 1 1 0 0 1 0 3 1 1 7 0 0 0 0 0 1 "English" 13 4 101 1 0 1 0 0 0 0 1 3 2 2 1 2 3 2 0 0 0 0 1 0 0 4 4 4 4 4 4 4 4 4 4 4 4 5 15 5 0 5 0 0 5 0 4 0 3 0 0 2 101 2 4 4 2 4 3 3 2 2 3 4 3 3 0 25 1 2 2 3 4 2 2 2 3 3 3)

  ,(Ans 6 6 4 3 4 3 3 4 2 4 4 3 2 3 3 2 4 1 4 1 2 2 2 4 4 1 1 0 0 0 5 1 1 0 0 0 0 0 0 1 "Business Administration" 12 4 2 1 1 1 0 1 0 0 1 1 101 1 101 101 101 2 1 0 0 0 0 0 0 3 3 2 2 2 1 2 3 2 2 2 1 20 30 5 5 5 5 3 3 5 5 5 5 5 1 4 4 2 4 4 4 5 5 5 4 5 5 5 5 4 1 25 1 4 5 3 3 1 2 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 3 1 2 3 3 3 3 3 3 2 1 2 1 2 1 3 4 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Biology" 15 3 2 1 1 1 1 0 0 0 1 3 1 2 1 3 2 2 0 0 0 0 0 0 0 4 4 2 2 3 3 3 3 1 1 2 3 3 25 5 0 0 0 5 4 0 0 0 0 0 5 3 4 3 3 3 4 2 3 4 3 4 3 4 3 3 0 25 2 5 4 4 3 2 3 4 1 1 1)

  ,(Ans 6 5 4 1 4 4 3 2 1 3 2 3 3 4 4 4 3 1 2 2 2 2 1 3 4 0 1 0 0 0 5 1 0 7 0 0 1 0 0 0 "Undecided" 13 2 101 1 0 1 0 0 0 0 1 6 1 1 1 1 3 2 0 1 0 0 0 0 0 3 2 3 3 3 3 3 2 2 3 2 1 5 15 5 5 5 0 0 0 1 1 0 5 2 0 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 0 25 1 1 99 2 3 4 2 2 3 3 3)

  ,(Ans 6 3 4 2 4 4 4 4 2 3 3 3 3 3 3 1 3 3 3 3 4 1 2 4 3 0 0 0 1 0 4 1 0 0 0 0 0 0 0 1 "Civil Engineering" 17 2 3 1 0 1 0 0 0 0 1 6 2 101 101 3 3 2 1 1 0 0 1 0 0 4 4 3 3 3 4 4 4 2 3 2 3 7 30 1 5 5 0 5 5 0 2 0 0 5 0 3 4 4 5 5 2 5 5 4 4 2 5 5 5 5 1 25 1 4 4 3 3 4 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 3 2 4 3 3 3 3 3 1 3 2 3 2 3 4 3 4 4 0 0 0 0 1 4 1 1 0 0 0 1 0 0 0 "Physics" 12 2 2 1 0 0 0 0 0 0 3 6 2 1 2 2 3 2 1 1 0 0 0 1 0 4 4 4 3 4 4 4 3 4 3 4 4 5 20 5 0 0 0 5 1 0 0 0 0 3 0 1 2 2 4 3 2 3 3 3 4 3 5 5 4 4 1 25 1 2 4 3 4 2 3 3 3 3 3)

  ,(Ans 6 2 4 1 4 4 4 4 1 3 1 2 3 4 1 4 3 1 3 3 2 1 1 2 1 1 0 0 0 0 5 1 1 5 0 0 1 0 0 0 "Psychology" 13 1 99 1 0 0 0 0 0 0 101 6 1 3 1 1 2 2 0 0 0 0 0 0 0 4 4 4 4 4 4 4 2 3 3 1 3 12 30 2 0 0 0 0 0 0 0 0 0 0 0 1 1 1 5 5 1 2 5 5 1 1 5 5 1 3 0 25 1 3 2 1 3 4 3 4 3 3 3)

  ,(Ans 6 3 4 2 3 4 4 4 3 3 4 4 4 4 3 4 3 2 3 2 2 2 1 3 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "History" 13 1 99 1 0 1 1 0 0 0 1 6 101 101 3 101 101 2 1 0 0 0 1 0 0 3 2 3 3 3 4 3 2 3 3 3 4 27 35 5 0 0 0 0 5 0 4 0 0 2 0 3 3 4 5 5 2 3 3 4 4 4 4 4 3 3 0 25 1 2 4 3 3 3 3 3 0 0 0)

  ,(Ans 3 5 3 101 3 4 4 4 3 3 3 3 3 3 3 4 3 1 3 2 3 2 1 3 3 0 0 0 1 0 4 1 0 99 0 1 1 1 0 0 "Undecided" 101 1 101 1 0 1 1 1 0 0 1 1 1 1 2 2 3 1 1 0 0 0 0 0 0 3 2 2 2 2 3 3 2 2 2 2 3 101 45 6 4 4 3 2 1 0 0 2 2 3 1 3 3 3 2 2 3 1 4 4 2 4 4 3 3 3 0 25 1 5 4 3 4 4 4 2 1 1 1)

  ,(Ans 6 1 4 3 4 4 4 4 4 4 4 4 3 4 4 4 4 3 4 4 4 4 3 2 4 0 1 0 0 0 3 1 1 0 1 0 0 0 0 0 "International Studies" 13 1 101 1 0 0 0 0 0 0 2 3 1 3 1 1 3 2 0 0 1 0 0 0 0 101 101 101 101 101 101 4 4 4 4 4 4 10 15 1 1 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 1 25 1 4 5 3 3 3 3 3 3 0 101)

  ,(Ans 6 3 4 1 3 4 4 4 4 3 2 1 4 2 1 4 2 1 2 1 1 1 1 2 1 0 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "History" 13 1 101 1 0 1 1 0 0 1 1 1 1 2 1 1 3 1 1 0 0 0 0 0 0 3 3 2 2 1 4 2 2 1 1 1 3 12 50 99 5 3 4 1 1 1 1 3 4 3 2 1 2 4 4 4 2 2 5 4 2 1 2 4 3 4 0 25 1 4 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 1 4 4 4 4 4 2 1 4 3 3 4 4 4 4 4 2 4 4 4 3 2 4 1 0 0 0 0 1 4 1 1 7 0 0 0 0 0 1 "Computer Engineering" 9 6 2 1 1 1 1 1 0 0 1 1 101 3 101 101 101 1 1 0 0 0 0 0 0 4 3 4 2 2 4 4 3 4 2 2 4 20 30 5 1 1 0 5 5 1 0 0 0 0 2 1 1 1 3 4 4 1 1 3 2 3 4 4 5 4 0 25 1 2 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 0 0 0 1 0 5 1 1 0 0 0 0 0 1 0 "Communication Studies" 16 6 2 1 1 1 0 0 0 0 1 5 1 3 1 1 1 2 0 1 0 0 0 1 0 3 2 2 2 2 2 3 3 3 3 3 3 15 30 5 1 1 1 1 3 2 1 2 1 2 1 2 2 2 2 2 3 3 3 3 3 3 3 5 5 5 1 24 2 4 3 2 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 3 4 4 3 1 1 4 1 4 4 4 1 1 3 1 3 1 1 1 1 1 2 0 0 0 1 0 4 0 1 0 0 0 1 0 0 0 "Electrical Engineering" 13 6 2 1 1 1 1 0 0 0 1 2 1 3 1 2 2 2 0 0 0 0 0 0 0 3 2 3 1 3 4 2 1 2 1 1 3 4 10 5 0 0 0 0 0 0 1 0 1 2 0 1 1 1 3 1 3 2 5 2 1 2 3 3 3 3 0 24 2 2 4 2 3 2 2 2 1 1 1)

  ,(Ans 6 2 4 2 2 4 4 4 1 2 3 3 1 4 3 3 4 2 4 4 4 1 2 3 4 1 1 0 0 0 3 1 1 7 0 0 0 0 0 1 "Political Science" 20 5 99 1 0 1 1 1 0 0 3 6 1 2 3 1 2 2 1 1 0 1 1 1 0 4 2 4 4 4 4 4 4 4 4 4 4 9 15 4 5 5 5 5 1 1 1 5 5 5 5 5 5 5 1 1 3 1 3 5 5 5 5 5 5 5 1 24 2 1 5 2 4 4 3 2 3 0 3)

  ,(Ans 6 3 4 1 4 3 3 4 3 4 3 4 3 2 3 4 1 1 1 1 1 1 1 1 4 0 0 0 1 0 4 1 0 7 0 0 0 0 0 1 "English" 16 5 2 1 0 1 0 0 0 0 1 6 2 101 101 101 3 2 0 0 0 0 0 0 0 3 3 3 2 3 3 2 2 3 2 2 3 5 45 99 3 2 5 0 1 0 3 0 0 2 3 2 2 2 5 4 1 4 1 2 3 2 3 3 2 3 0 24 1 5 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 3 3 2 1 4 4 1 4 2 3 1 2 1 3 1 4 1 1 4 4 0 0 0 1 0 4 0 1 7 0 0 0 0 0 1 "Undecided" 12 4 101 0 0 1 1 0 1 0 1 5 2 1 1 1 3 2 1 0 0 0 1 1 0 4 2 3 2 3 4 3 3 2 1 4 4 25 30 5 0 0 0 0 5 0 3 0 2 0 0 1 2 1 5 5 1 5 3 4 2 2 3 3 99 99 0 24 1 2 2 3 3 2 2 3 3 0 3)

  ,(Ans 99 4 3 1 4 2 3 4 3 1 2 1 1 3 2 1 1 1 1 1 3 2 2 1 1 0 1 0 0 0 4 1 1 7 0 0 0 0 0 1 "Communication Studies" 15 4 2 1 1 1 1 1 0 1 1 2 1 3 2 1 2 2 0 0 0 0 0 0 0 4 4 4 4 4 4 4 3 3 2 2 1 25 45 5 5 5 5 5 5 2 5 5 5 2 5 3 5 4 5 5 3 5 2 4 5 4 4 4 3 4 0 24 2 2 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 4 1 4 4 3 4 1 1 1 2 4 3 3 1 2 1 3 1 1 3 1 4 4 1 1 0 0 0 5 0 1 99 0 0 1 0 0 0 "Psychology" 13 4 2 1 0 1 0 0 0 0 1 2 1 3 1 1 2 2 0 0 0 0 0 0 0 4 3 3 1 1 3 3 2 2 1 1 1 10 20 5 0 0 0 0 0 0 0 0 0 0 0 1 1 1 3 3 1 3 1 3 3 2 3 3 2 2 0 24 2 5 4 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 3 2 4 4 3 3 3 4 4 3 2 4 4 4 3 3 4 2 2 1 2 2 3 1 0 0 0 0 2 0 0 7 0 0 0 0 1 0 "Electrical Engineering" 13 2 2 1 0 0 0 0 0 0 1 5 2 3 1 1 1 1 0 0 0 0 0 0 0 3 2 2 2 2 2 2 2 2 2 2 2 5 20 0 1 1 2 0 5 2 3 0 0 2 0 3 3 3 3 3 3 3 2 2 2 3 3 3 3 3 0 24 1 3 3 3 4 4 2 3 1 1 1)

  ,(Ans 6 3 4 4 4 4 4 4 3 4 2 1 3 4 1 1 4 1 4 4 4 4 4 4 1 0 0 0 1 0 5 0 0 99 0 0 0 0 0 1 "Social Science" 5 2 2 0 0 0 0 0 0 1 1 1 3 101 101 3 3 2 0 1 0 0 0 0 0 4 4 4 4 4 4 4 2 1 1 1 3 2 25 5 5 5 5 4 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 1 24 2 5 5 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 3 4 4 4 2 1 3 1 4 2 2 2 1 1 3 0 0 0 0 1 4 1 1 7 0 0 1 0 0 0 "Architecture" 15 2 2 1 0 0 0 0 0 0 1 1 101 101 101 101 3 2 1 0 0 0 0 0 0 3 3 4 4 4 4 3 3 4 3 2 3 3 50 1 1 5 5 1 1 4 5 0 4 2 5 2 4 4 3 4 4 4 4 4 5 4 4 5 3 3 1 24 2 3 3 3 3 3 3 3 0 0 0)

  ,(Ans 6 2 4 3 3 4 3 4 2 1 2 2 2 4 2 2 2 1 3 3 2 2 1 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Music" 14 1 99 0 0 1 0 0 0 0 3 2 3 101 101 101 101 3 0 0 0 0 0 0 0 4 3 3 3 3 3 4 3 3 3 3 3 101 101 5 0 0 0 2 3 0 0 0 0 0 0 2 3 3 4 4 2 4 2 3 3 3 4 4 1 2 0 24 1 4 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 5 3 1 4 4 4 3 1 2 3 2 3 3 2 3 3 2 3 3 3 2 2 3 2 0 0 0 1 0 4 1 1 0 0 0 1 0 1 0 "Art" 15 1 101 1 0 1 1 0 0 1 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 2 3 16 40 3 5 5 5 5 2 5 5 2 5 5 5 2 1 3 2 2 3 2 1 3 3 3 4 3 4 4 0 24 1 5 5 3 4 4 4 2 3 0 0)

  ,(Ans 6 3 4 4 4 4 4 3 3 4 4 2 3 4 4 4 4 4 4 4 3 4 3 3 2 0 1 0 0 0 4 1 1 3 0 0 0 0 1 0 "Computer Science" 30 1 101 1 0 1 1 0 0 0 1 3 2 101 101 101 101 2 0 0 0 0 0 1 0 4 4 101 3 3 4 101 101 101 101 101 101 5 5 4 5 3 3 4 3 101 2 3 2 2 2 2 1 1 1 1 3 1 1 2 2 2 5 5 5 5 1 24 1 2 2 3 3 2 3 3 0 0 0)

  ,(Ans 5 3 3 2 4 4 4 3 2 4 3 2 4 3 3 1 3 1 3 1 2 1 3 2 2 0 0 0 0 1 2 1 1 0 0 0 0 0 1 0 "Sociology" 20 1 101 1 0 1 0 1 0 0 1 6 3 2 1 2 1 2 1 1 0 0 0 0 0 4 3 4 3 3 3 3 2 4 3 3 2 2 15 5 3 5 1 5 3 0 0 1 4 5 1 4 5 2 4 4 3 3 3 5 5 5 5 4 3 3 0 24 2 2 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 4 2 4 4 4 1 4 4 3 1 4 1 4 3 4 4 4 4 1 0 0 0 1 0 4 1 1 7 0 0 1 0 0 0 "Accounting" 16 3 1 1 0 1 0 1 0 0 1 3 3 101 101 101 3 2 0 0 0 0 0 1 0 4 1 4 101 101 101 101 101 101 3 3 3 2 10 99 4 5 5 5 3 1 0 3 3 5 5 3 3 2 3 4 4 3 3 3 4 3 5 5 5 5 1 24 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 4 3 2 3 1 3 2 1 2 4 1 1 2 1 2 2 1 3 2 2 4 0 0 0 1 0 3 1 1 0 0 0 1 0 0 0 "Music" 14 6 2 1 0 1 0 0 0 0 1 6 3 1 1 1 1 2 0 0 0 0 0 0 0 4 3 3 2 3 3 3 1 1 1 3 1 0 5 5 1 1 0 1 2 0 0 2 1 2 1 4 3 3 4 4 3 2 5 5 3 4 3 4 1 99 0 23 1 3 1 3 4 4 3 4 3 0 3)

  ,(Ans 6 6 4 3 4 4 3 2 2 4 2 3 4 2 2 2 2 2 2 2 2 2 2 2 4 0 1 0 0 0 5 0 1 7 0 1 0 0 0 0 "Physics" 12 6 2 1 0 0 0 0 0 0 101 2 3 2 1 1 1 3 0 0 0 0 0 0 0 4 4 4 4 4 4 4 2 2 2 2 2 101 101 2 5 5 5 5 5 1 3 1 3 5 5 3 3 3 2 2 2 3 3 3 3 3 3 3 3 3 0 23 1 2 2 1 4 2 2 3 1 0 0)

  ,(Ans 6 4 3 1 4 4 4 4 1 3 2 4 4 1 1 3 1 1 3 1 1 1 1 1 1 0 1 0 0 0 4 1 1 7 0 0 0 0 0 1 "Art" 12 6 2 1 0 0 0 0 0 0 3 1 1 3 1 1 1 1 0 0 0 0 0 0 0 4 3 3 3 3 4 4 2 3 3 4 4 15 35 5 5 5 5 3 3 1 4 4 5 1 5 5 5 5 5 4 2 5 2 5 2 5 4 4 3 3 1 23 2 2 2 1 4 3 3 3 3 3 3)

  ,(Ans 5 2 4 1 4 3 3 4 1 4 2 3 4 4 3 3 3 2 2 2 3 1 1 1 3 1 0 0 0 0 4 1 1 7 0 0 0 0 1 0 "Did not answer" 15 6 2 1 0 1 1 1 0 0 1 6 1 1 2 1 3 2 1 1 0 0 0 0 0 4 3 3 4 4 3 4 2 2 3 4 2 20 30 5 0 0 0 0 0 0 0 0 0 0 0 1 2 2 5 4 3 5 3 3 3 3 5 4 4 5 1 23 1 2 2 3 3 4 4 4 1 1 1)

  ,(Ans 6 5 4 1 2 4 4 3 1 2 3 3 2 4 2 2 2 1 2 2 3 1 1 3 2 1 0 0 0 0 3 1 1 99 0 0 0 1 0 0 "Did not answer" 12 5 2 1 1 1 1 0 0 0 1 1 1 1 1 3 2 2 0 0 0 0 0 0 0 4 3 3 2 2 3 4 3 3 1 1 3 7 30 5 5 5 2 2 0 0 2 0 4 5 5 3 3 3 2 2 3 2 3 3 4 4 4 4 4 4 0 23 1 4 4 3 4 3 3 4 1 1 1)

  ,(Ans 6 5 4 1 3 4 4 3 3 3 4 3 4 2 2 3 3 2 3 3 2 3 2 3 3 0 0 0 1 0 3 0 0 0 0 0 1 0 0 0 "Undecided" 12 5 2 1 0 1 1 0 0 0 1 6 3 1 1 1 2 2 0 0 0 0 0 0 0 4 2 3 2 2 2 4 1 1 1 1 1 1 15 5 5 5 5 0 1 1 1 3 3 1 1 3 3 3 4 4 3 3 3 3 3 3 4 4 2 2 0 23 1 4 2 3 3 3 3 4 0 0 1)

  ,(Ans 6 3 4 1 3 4 4 4 1 2 2 3 4 2 2 4 3 1 4 1 2 1 3 3 3 1 0 0 0 0 4 1 1 7 1 1 0 0 0 0 "History" 15 4 2 1 0 1 1 1 0 0 1 6 2 2 2 2 3 2 0 0 0 0 0 0 0 4 2 4 2 2 2 4 1 4 2 2 1 1 10 5 0 0 0 0 5 0 0 0 0 5 0 3 4 2 3 3 2 1 2 4 4 3 4 3 1 99 0 23 1 2 2 3 3 3 3 2 1 1 1)

  ,(Ans 3 5 1 1 4 3 3 3 1 1 3 2 1 3 1 2 3 1 3 1 4 2 1 2 1 0 0 0 0 0 3 1 1 1 0 0 0 0 0 1 "Undecided" 9 4 2 1 1 1 1 1 0 0 1 1 2 1 1 3 3 2 1 1 0 0 0 0 0 4 3 4 3 3 3 4 2 4 4 3 4 101 20 5 5 5 3 5 4 1 2 4 5 5 5 3 3 3 3 4 3 2 4 4 4 3 5 4 4 4 99 23 1 2 2 3 2 2 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 4 2 2 4 4 3 3 1 1 1 1 1 3 1 1 1 3 2 4 0 0 0 0 0 5 1 1 7 0 0 0 0 1 0 "Business Administration" 17 4 1 1 1 1 1 1 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 1 2 2 4 4 4 1 2 2 101 101 5 5 5 5 3 2 3 3 3 3 3 1 4 4 4 5 5 4 3 3 4 3 4 3 3 3 3 0 23 1 2 2 3 4 3 3 3 1 1 1)

  ,(Ans 6 4 4 1 4 4 4 4 1 3 4 1 4 3 4 4 3 1 1 1 1 1 1 1 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Liberal Studies" 13 3 2 1 0 1 0 0 0 0 1 2 2 1 1 1 3 1 1 0 0 0 0 0 1 4 4 4 2 3 4 3 3 3 1 3 4 20 60 5 0 0 0 0 1 0 0 2 3 0 0 4 4 5 4 3 2 3 3 4 1 3 3 3 3 3 0 23 2 2 1 3 4 4 4 2 3 3 0)

  ,(Ans 6 6 3 4 4 4 4 1 4 4 2 4 4 4 4 4 2 2 3 3 2 3 3 3 3 0 0 0 1 0 3 0 0 0 1 1 1 0 1 0 "Electrical Engineering" 12 3 2 1 0 1 0 0 0 0 1 6 1 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 2 3 2 3 3 3 2 3 2 22 65 0 0 0 0 0 0 0 0 0 0 0 0 1 2 1 1 1 1 1 1 1 1 1 99 99 99 99 0 23 1 1 1 3 4 3 3 3 3 0 0)

  ,(Ans 6 5 4 1 4 4 3 3 2 3 1 3 4 3 1 4 2 1 2 2 2 1 2 4 4 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Foreign Languages" 16 2 2 1 0 1 1 0 0 0 1 1 1 1 1 3 2 2 0 1 0 0 0 0 0 4 4 4 4 4 4 4 4 4 3 3 4 5 35 5 0 4 0 1 1 0 0 0 2 0 0 1 2 3 3 1 3 2 3 5 3 4 4 4 2 4 1 23 2 2 2 3 3 2 3 3 1 1 1)

  ,(Ans 6 6 4 2 2 4 4 1 1 2 2 4 4 3 2 1 4 1 3 3 4 1 1 2 4 0 1 0 0 0 4 1 1 0 0 0 0 0 1 0 "Did not answer" 21 2 2 1 1 1 1 1 0 0 1 6 2 1 1 2 3 2 1 1 0 0 1 1 0 4 4 4 4 4 4 4 4 4 4 3 4 4 25 5 0 1 0 2 5 5 5 0 1 2 0 1 1 1 4 5 3 3 1 2 4 1 5 5 5 5 1 23 2 2 5 2 4 2 3 2 1 1 1)

  ,(Ans 101 3 4 4 4 4 4 3 4 4 1 1 4 1 1 1 3 1 1 2 1 4 3 3 3 1 0 0 0 0 3 1 1 1 1 0 0 0 1 0 "Business Administration" 16 2 1 1 0 0 1 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 1 4 4 3 3 4 1 4 4 101 101 2 5 5 5 5 5 5 5 5 5 5 5 2 2 2 2 2 2 2 2 2 2 2 99 5 5 5 0 23 1 4 4 3 3 3 3 3 0 0 0)

  ,(Ans 6 5 101 3 4 4 4 4 3 3 3 2 3 4 4 3 2 2 3 3 3 3 2 2 2 1 0 0 1 1 4 1 1 1 1 0 0 0 1 0 "Computer Science" 15 1 101 1 0 1 1 1 0 0 1 3 3 101 1 1 3 3 1 0 0 0 0 1 0 4 3 3 3 3 3 3 3 3 3 3 3 101 101 4 2 4 5 5 2 5 4 1 3 2 3 2 3 5 3 3 3 3 3 3 3 3 3 3 3 3 1 23 1 2 5 2 2 2 2 2 0 0 0)

  ,(Ans 6 4 4 4 3 4 4 4 1 4 2 4 4 4 3 1 3 1 3 3 4 1 1 3 4 0 0 1 0 0 5 1 1 4 0 0 0 0 1 0 "Architecture" 15 1 99 1 0 1 0 0 0 0 1 2 3 1 1 1 3 2 1 0 0 0 0 0 0 4 3 3 3 3 4 4 3 3 2 3 4 0 5 5 5 5 1 5 5 5 5 3 5 5 5 4 3 3 5 5 5 5 3 5 3 3 4 4 101 4 0 23 2 3 2 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 2 101 4 2 4 4 101 101 3 3 2 4 4 4 4 1 2 4 0 0 0 1 0 5 1 1 3 0 0 0 0 1 0 "" 17 1 101 1 0 1 1 1 0 0 2 2 1 3 1 1 1 2 0 0 0 0 0 1 1 4 3 101 2 2 2 4 3 101 2 3 3 0 20 6 5 3 0 0 5 5 4 3 5 4 4 2 3 3 4 4 2 2 4 2 5 101 5 5 5 5 0 23 2 2 2 3 3 4 3 3 0 0 0)

  ,(Ans 6 5 3 1 1 1 3 2 3 2 2 2 1 1 1 1 1 1 1 1 3 2 1 3 2 1 0 0 1 0 2 0 1 0 0 1 0 0 0 0 "Biology" 12 6 101 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 1 4 4 3 3 2 4 2 1 2 1 1 1 101 101 0 0 0 0 5 3 5 5 0 3 0 5 1 1 1 4 4 5 2 1 2 2 1 2 5 3 2 1 22 1 5 5 3 4 4 2 2 0 0 0)

  ,(Ans 6 6 4 1 4 4 4 2 1 3 3 4 4 3 1 1 3 1 4 2 4 1 2 3 4 1 0 1 0 0 4 1 1 0 1 0 0 0 0 0 "English" 12 6 2 1 1 1 1 1 0 0 1 6 2 2 1 3 2 2 0 1 0 0 0 0 0 4 4 4 4 4 4 4 2 4 3 4 2 6 15 5 0 1 0 0 5 3 3 5 5 5 0 3 4 2 5 5 4 3 3 3 4 4 4 4 3 3 1 22 2 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 2 3 4 4 3 1 2 3 2 2 4 3 2 3 2 3 3 3 2 2 4 2 1 0 0 0 0 5 1 1 0 0 0 0 0 1 0 "Film" 13 5 2 1 1 0 0 1 0 0 1 1 3 1 1 2 3 2 1 1 0 0 0 0 0 4 4 4 4 4 4 3 2 3 3 4 3 1 5 5 5 5 2 4 2 3 5 1 4 1 2 2 3 2 4 4 5 5 3 4 4 3 5 5 4 4 1 22 2 3 5 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 2 4 4 3 2 2 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 1 3 0 1 7 0 1 1 0 0 0 "Undecided" 8 4 2 1 0 1 1 0 0 0 1 6 1 1 1 2 3 1 0 0 0 0 0 1 0 3 4 1 3 3 3 3 4 1 2 3 3 10 60 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 3 3 2 3 0 22 1 2 2 3 3 2 3 4 0 0 0)

  ,(Ans 6 5 2 1 2 4 4 4 4 1 4 4 3 2 2 1 3 3 4 1 4 1 1 3 3 1 0 0 0 0 3 0 1 0 0 0 0 0 0 1 "Biology" 15 4 99 1 0 1 1 1 0 0 1 2 3 1 1 1 3 2 0 0 0 0 1 0 0 4 3 3 4 3 4 3 1 2 1 2 4 20 40 99 0 0 0 5 5 2 5 2 5 5 0 3 3 2 3 2 4 4 4 5 2 2 5 4 3 3 1 22 1 5 5 3 2 1 2 3 3 0 0)

  ,(Ans 6 4 3 1 4 4 3 4 1 1 2 3 3 4 3 1 3 1 3 1 1 1 1 3 2 1 0 0 1 0 4 1 1 7 0 0 0 0 1 0 "Geology" 18 3 2 1 0 1 1 0 0 0 1 2 1 2 1 2 3 2 1 0 0 0 0 0 0 4 4 4 3 3 3 4 4 101 2 2 101 10 30 5 0 0 0 3 5 0 0 0 0 0 0 2 2 2 5 5 2 2 2 5 4 4 5 5 5 5 0 22 2 2 2 3 4 2 3 1 1 0 1)

  ,(Ans 6 3 4 3 4 3 3 3 2 2 2 2 3 3 2 3 2 1 3 2 3 3 1 3 2 0 0 0 1 0 4 0 1 2 0 0 0 0 0 1 "Mechanical Engineering" 13 3 101 1 0 0 0 0 0 0 1 1 2 101 101 3 101 2 1 0 0 0 0 0 0 3 3 4 3 4 3 3 3 4 3 4 3 2 10 99 5 3 4 1 4 5 5 3 4 3 5 5 3 3 4 4 4 4 4 4 3 3 4 4 5 5 1 22 1 4 4 3 4 4 4 2 0 0 0)

  ,(Ans 6 2 4 2 4 4 3 4 1 2 3 2 4 4 4 4 3 1 3 3 3 1 1 1 1 1 0 0 0 0 4 0 1 0 0 0 0 0 0 0 "Biology" 15 3 2 1 1 1 1 1 0 0 1 5 2 2 101 3 101 2 1 0 0 0 0 0 0 4 4 4 3 3 4 3 4 4 3 2 4 3 25 0 3 3 2 4 5 5 5 5 5 5 5 3 4 2 5 5 5 5 5 5 4 5 5 5 3 4 0 22 1 5 2 3 3 2 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 3 2 4 2 2 4 2 4 4 3 3 2 3 2 0 1 0 0 0 3 1 0 0 1 0 0 0 0 0 "Business Administration" 101 3 2 1 0 1 1 0 0 0 1 1 2 3 1 1 1 2 1 1 0 0 0 0 0 4 3 3 3 3 4 3 3 4 2 2 4 7 20 5 1 2 1 1 1 0 0 0 0 0 2 1 1 1 1 1 1 1 3 2 2 3 3 3 3 3 0 22 1 2 5 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 3 3 4 3 3 2 2 3 3 2 2 3 3 2 2 2 2 3 3 2 2 3 3 1 0 0 0 0 4 1 0 0 1 0 0 0 0 0 "Engineering" 15 3 1 1 0 0 0 0 0 0 1 1 2 2 2 2 3 3 1 0 0 0 0 0 0 101 101 101 101 101 101 101 101 101 101 101 101 101 101 5 5 4 101 3 3 4 3 3 2 3 3 2 2 3 4 4 2 3 3 2 3 3 3 4 4 4 0 22 1 2 5 3 2 1 3 1 0 1 1)

  ,(Ans 6 6 3 2 3 4 3 3 2 4 4 4 4 4 2 1 3 1 101 2 2 1 2 2 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Business Administration" 15 3 2 1 0 1 1 1 0 0 1 5 3 1 1 1 1 2 0 0 0 0 0 1 0 4 2 3 3 3 3 3 2 2 3 101 2 1 10 5 0 5 5 5 5 5 5 5 5 5 1 3 5 3 4 5 5 5 5 5 3 5 4 4 3 4 1 22 1 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 101 1 3 1 3 101 3 1 1 101 3 3 2 1 3 1 1 1 1 1 1 1 1 0 0 0 0 0 5 1 1 101 0 0 0 0 0 0 "Music" 15 3 2 1 1 1 1 1 0 0 1 6 2 1 1 2 3 99 1 0 0 0 0 0 0 3 3 1 1 3 4 3 1 1 1 2 4 4 15 99 2 2 3 101 5 2 3 3 3 5 2 3 3 3 4 4 4 3 3 4 3 5 5 5 2 3 0 22 1 2 1 3 4 3 3 3 1 1 1)

  ,(Ans 6 4 4 1 4 4 4 4 3 4 1 2 1 4 3 1 2 1 4 1 2 2 1 3 4 1 0 0 0 0 5 101 1 7 0 0 0 0 0 1 "Foreign Languages" 14 3 2 1 0 1 0 0 0 0 1 6 3 1 1 1 3 2 0 0 0 0 0 0 0 4 3 3 3 3 4 2 1 1 1 1 2 5 35 2 5 5 5 4 2 0 0 5 5 3 5 5 5 5 5 3 4 1 4 5 2 5 3 4 2 3 0 22 2 2 5 3 3 4 3 3 1 1 1)

  ,(Ans 6 6 4 1 4 4 4 2 3 3 3 3 4 4 4 4 4 1 4 3 3 3 3 3 2 1 0 0 0 0 5 1 1 101 0 0 0 0 0 0 "English" 12 2 2 1 0 0 0 0 0 0 1 1 3 2 2 1 3 1 0 0 0 0 0 0 0 4 4 4 4 4 4 4 3 3 2 2 4 10 30 5 1 2 0 5 5 0 0 0 0 2 0 4 5 3 5 5 3 2 3 3 4 5 4 4 4 4 0 22 2 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 4 1 1 1 1 4 1 1 1 1 1 2 2 3 3 3 2 2 4 3 4 4 4 0 0 0 0 1 1 0 0 4 0 0 1 0 0 0 "Engineering" 30 2 3 0 0 0 0 0 1 0 3 1 1 3 3 3 1 1 0 1 0 0 0 0 0 2 2 2 2 2 2 101 101 101 101 101 101 13 1 3 3 4 2 2 1 0 1 2 3 4 5 1 2 3 4 5 4 3 2 1 2 3 5 4 3 2 0 22 1 1 1 1 3 4 3 2 1 1 1)

  ,(Ans 6 3 4 3 4 4 4 4 4 3 4 1 3 3 3 3 3 3 4 3 4 4 2 3 1 0 1 0 0 0 4 1 1 1 0 0 0 1 0 0 "Accounting" 17 1 101 1 0 1 1 1 0 0 3 3 3 2 1 1 1 3 0 0 0 0 0 1 0 101 101 101 101 101 101 3 3 3 4 3 4 101 101 5 5 5 4 4 4 4 5 2 1 2 5 3 3 3 3 4 4 4 3 3 5 5 5 5 5 5 0 22 1 4 5 3 3 3 3 3 0 0 0)

  ,(Ans 6 1 4 101 4 4 3 3 4 3 2 1 4 4 3 2 2 2 3 2 1 3 3 2 1 0 0 0 0 1 4 1 1 0 0 1 1 0 0 0 "Accounting" 16 1 99 1 0 0 0 0 0 0 3 3 101 101 101 101 2 2 0 0 0 0 0 1 0 4 3 3 2 1 3 3 3 3 1 1 3 2 10 6 5 5 2 3 1 3 5 0 0 3 1 4 4 2 3 2 4 3 1 1 3 2 4 2 1 99 1 22 1 2 2 3 2 1 3 2 0 0 0)

  ,(Ans 6 6 2 1 1 1 3 3 4 4 2 1 4 3 4 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 2 1 1 0 0 1 1 1 0 0 "Biology" 12 1 101 1 0 1 1 1 0 0 1 1 3 1 3 1 3 3 1 0 0 0 0 0 0 4 3 3 3 101 4 4 3 3 2 2 2 101 101 99 5 5 5 2 5 5 4 101 5 0 5 1 2 2 3 3 3 101 101 5 2 3 99 99 99 99 1 22 2 4 99 3 4 4 4 3 1 1 1)

  ,(Ans 5 5 2 1 4 4 4 4 4 4 3 1 1 3 3 2 1 1 1 1 1 1 1 1 1 1 1 0 0 0 3 0 0 0 0 1 1 0 1 0 "Undecided" 13 1 101 1 0 1 1 1 0 0 1 1 1 3 1 1 1 1 1 0 0 0 1 0 0 4 4 3 3 3 4 3 3 3 2 3 3 26 35 4 5 4 3 5 3 2 3 2 4 1 5 3 2 2 4 3 3 3 3 3 3 4 3 4 4 3 0 22 1 4 4 3 4 4 3 3 0 0 3)

  ,(Ans 6 3 3 2 2 3 3 3 4 3 4 2 3 4 3 4 3 3 3 1 4 4 4 3 2 0 0 0 1 0 3 0 1 99 0 0 1 0 0 0 "Electrical Engineering" 16 1 101 1 0 1 0 1 0 0 2 3 101 3 101 101 2 2 0 0 0 0 0 0 0 4 4 101 101 101 101 101 101 3 1 3 2 7 15 3 5 4 5 2 4 5 5 5 5 0 5 3 3 3 3 4 4 4 3 3 3 3 5 3 3 3 1 22 1 2 4 3 3 2 3 3 0 0 0)

  ,(Ans 6 3 4 2 4 4 3 4 2 3 2 4 4 3 3 3 3 1 3 1 3 1 1 2 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "English" 9 101 2 1 0 0 0 0 0 0 1 2 3 1 1 2 3 2 0 0 0 0 0 0 0 4 3 2 3 2 4 4 1 1 1 1 3 2 20 5 0 0 0 0 0 0 0 0 0 0 0 3 3 1 3 3 4 2 2 4 3 3 3 3 3 3 0 21 2 2 99 3 3 3 3 3 1 2 1)

  ,(Ans 6 4 4 2 4 4 4 4 4 3 2 4 4 3 3 1 3 1 3 1 1 1 1 1 1 0 0 0 0 1 5 0 0 7 0 0 0 0 1 0 "Did not answer" 20 6 2 1 0 0 0 0 0 0 1 3 3 101 101 2 101 99 1 0 0 0 0 0 0 4 3 2 2 2 4 3 2 1 1 1 3 1 10 99 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 3 2 5 5 4 5 3 4 3 3 1 21 2 5 4 101 3 3 3 3 3 3 3)

  ,(Ans 3 6 3 4 4 4 4 2 1 3 4 4 4 4 4 4 4 2 4 3 4 2 2 3 4 0 1 0 0 0 4 1 1 7 0 1 1 0 0 0 "Foreign Languages" 8 5 2 1 1 1 0 1 0 0 1 6 101 3 101 101 3 1 0 0 0 0 0 1 0 4 2 2 2 2 2 101 101 101 101 101 101 20 35 5 1 3 1 0 0 1 5 0 0 0 0 1 3 3 3 3 2 2 1 1 1 2 2 1 1 1 0 21 2 3 3 3 1 1 2 1 3 0 3)

  ,(Ans 6 4 4 3 4 4 4 4 1 4 3 4 4 4 3 4 4 3 4 3 2 1 1 1 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Biology" 15 5 2 1 0 0 0 0 0 0 1 6 1 2 2 1 2 1 1 0 0 0 0 0 0 3 3 3 3 3 3 3 2 3 3 3 3 30 45 5 5 5 101 3 5 5 5 5 5 5 3 3 3 5 5 5 5 4 4 4 4 4 3 3 3 3 0 21 1 1 2 3 3 2 2 3 0 0 0)

  ,(Ans 6 6 4 1 4 4 4 1 2 4 1 4 4 4 4 1 4 2 4 3 3 1 1 3 4 1 0 0 0 0 5 0 1 7 0 0 0 0 0 1 "Art" 12 5 2 1 1 1 0 0 0 0 3 2 1 2 1 1 3 2 0 0 0 0 0 0 0 4 2 2 2 3 2 4 2 2 2 3 2 18 50 99 4 4 2 0 5 2 5 0 2 5 0 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 0 21 1 3 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 3 4 3 3 3 2 2 3 3 1 1 2 2 1 2 1 2 1 1 1 1 0 0 0 0 1 4 0 1 0 0 0 0 0 0 1 "Music" 12 5 2 1 0 1 0 0 0 0 1 6 101 3 101 101 2 2 0 1 0 0 0 0 0 4 3 4 4 4 3 3 2 2 1 3 3 10 15 99 5 5 5 5 3 3 5 4 5 4 5 2 4 3 3 3 3 5 4 3 2 3 4 3 2 3 1 21 2 5 5 3 4 3 3 3 3 3 3)

  ,(Ans 99 1 4 4 4 4 3 2 3 4 2 1 1 1 1 4 1 4 1 1 1 1 4 1 1 0 1 0 0 0 4 1 0 7 0 0 1 0 0 0 "Business Administration" 13 4 2 1 0 1 0 0 0 0 1 2 1 3 1 1 1 2 1 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 11 17 5 5 5 5 5 5 3 4 4 5 5 1 3 4 3 4 101 3 2 3 4 4 5 4 4 3 3 1 21 1 4 5 3 3 3 2 4 3 0 3)

  ,(Ans 6 3 4 1 4 4 3 1 1 4 4 3 4 2 2 1 1 1 2 1 1 3 1 4 4 1 0 0 1 0 3 1 1 0 1 0 0 0 0 0 "Undecided" 12 4 2 1 0 1 0 0 0 0 1 6 101 101 101 101 3 2 1 0 0 1 0 0 0 4 3 2 3 4 2 3 2 2 3 4 3 6 22 5 1 1 0 2 5 0 0 0 0 0 0 4 4 2 5 5 2 1 2 5 2 3 3 101 99 99 0 21 2 2 2 1 3 1 3 3 3 3 3)

  ,(Ans 6 2 4 1 4 4 3 4 3 3 4 3 4 3 3 2 3 2 3 2 2 2 2 3 2 0 1 0 0 0 5 1 1 0 0 0 0 1 0 0 "Biology" 15 4 2 1 0 1 1 0 0 0 1 1 2 1 1 3 3 2 1 1 0 0 0 1 0 4 4 3 3 3 4 4 4 3 3 2 4 10 35 6 5 5 5 5 4 5 5 5 5 5 5 4 3 3 3 3 4 4 4 3 2 3 4 4 3 4 1 21 1 4 5 3 2 2 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 4 4 4 3 4 3 2 4 4 4 4 4 1 2 3 3 3 3 3 4 0 1 0 0 0 3 1 1 101 0 1 1 0 0 0 "Architecture" 13 4 2 1 0 1 1 1 0 0 1 101 2 101 101 101 3 1 0 0 0 0 0 0 0 4 101 101 3 2 4 101 2 3 101 101 101 101 45 5 1 4 101 0 0 0 0 1 2 0 0 1 1 1 1 1 1 2 2 1 1 2 4 5 3 3 0 21 2 2 2 3 4 4 2 2 2 2 2)

  ,(Ans 6 3 4 2 4 3 3 4 1 4 2 4 3 3 2 2 2 1 2 1 1 1 3 2 4 0 1 0 0 0 4 0 1 0 1 0 0 0 0 0 "Child and Family Studies" 16 4 2 1 0 0 0 0 0 0 1 6 1 1 2 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 2 2 2 2 2 2 5 90 3 5 5 5 5 5 1 4 4 4 3 5 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 1 21 2 2 2 2 4 4 3 4 1 2 1)

  ,(Ans 6 6 4 3 4 4 4 4 4 4 4 4 3 4 4 4 4 3 4 4 4 3 4 4 4 1 1 0 1 0 5 1 1 0 1 0 0 1 0 0 "Health Studies" 15 3 2 1 0 1 0 0 0 0 1 2 3 1 2 3 3 1 0 1 0 0 0 1 0 2 101 101 101 101 101 3 4 3 3 3 3 30 60 101 5 5 5 5 4 4 4 4 4 101 101 4 101 5 5 4 101 4 5 4 4 5 5 5 5 5 1 21 1 2 3 3 2 2 3 2 0 0 0)

  ,(Ans 6 3 4 2 2 2 4 3 1 1 2 3 2 101 3 2 3 1 3 2 2 2 2 4 3 1 0 0 1 0 3 1 1 4 0 0 0 0 0 1 "History" 12 3 99 1 1 1 1 1 0 0 1 1 1 3 2 1 2 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 2 2 2 3 2 10 20 5 0 3 0 3 3 3 2 2 2 0 2 1 3 1 5 5 3 3 3 3 3 3 3 3 3 3 1 21 1 4 4 2 1 2 2 2 3 3 3)

  ,(Ans 6 2 101 1 4 4 4 4 3 3 4 4 4 2 2 1 1 1 1 1 1 3 1 2 1 0 1 0 0 0 4 1 1 0 0 0 0 0 0 0 "Science" 16 3 2 1 0 1 1 1 0 0 1 1 1 1 2 2 3 2 0 1 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 8 25 5 0 4 4 3 5 3 4 4 4 0 5 2 2 2 4 3 3 3 3 2 2 3 5 5 4 5 1 21 2 2 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 2 2 2 3 2 2 2 3 2 3 2 2 3 2 2 2 1 2 2 1 2 2 0 0 0 0 1 4 1 1 99 0 0 0 0 0 1 "Administration of Justice" 16 3 2 1 1 1 1 1 0 0 1 2 1 2 2 2 3 2 0 0 0 0 0 0 0 3 3 3 3 3 4 2 1 1 1 1 3 5 10 5 5 5 5 1 0 2 0 4 3 2 5 1 3 3 3 3 3 3 3 3 4 4 3 3 3 3 1 21 2 99 2 3 4 3 2 2 3 0 3)

  ,(Ans 6 6 4 1 4 4 101 2 2 3 4 4 4 4 4 4 4 2 4 4 4 2 4 4 4 1 1 0 0 0 4 1 0 7 0 0 0 0 0 1 "Art" 14 2 2 1 0 1 1 1 0 0 1 6 1 1 2 101 3 1 1 1 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 18 45 5 0 0 101 3 3 0 0 0 0 5 5 2 2 2 5 5 2 3 2 2 3 4 4 5 4 5 1 21 1 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 101 3 101 101 4 101 101 101 101 101 4 4 101 101 101 4 1 101 101 101 101 3 1 0 0 0 0 4 0 0 7 0 1 1 0 0 0 "Foreign Languages" 12 2 2 1 0 0 0 0 0 0 1 6 3 101 101 101 2 2 0 0 0 0 0 0 0 4 2 4 3 101 101 101 101 101 101 3 4 2 30 4 5 5 5 4 5 4 5 5 5 3 5 4 4 3 4 3 3 4 3 4 3 3 3 3 3 3 0 21 2 2 1 3 2 2 3 3 3 0 3)

  ,(Ans 6 6 101 1 3 4 4 1 1 3 4 2 3 2 3 1 3 1 1 1 1 1 1 4 1 0 1 0 0 0 5 1 1 0 0 0 0 0 0 1 "Undecided" 15 2 2 1 0 1 1 0 0 1 1 6 2 1 1 1 3 2 1 0 0 0 0 0 0 4 3 4 2 3 4 3 2 2 3 3 4 2 40 5 0 3 3 0 0 2 0 3 3 0 5 3 4 4 4 4 5 2 5 5 5 4 4 4 3 3 0 21 1 2 3 3 3 4 3 3 1 2 1)

  ,(Ans 6 4 4 2 3 4 3 3 1 2 2 2 4 4 1 1 3 1 3 2 3 1 1 3 2 0 1 0 0 0 4 1 1 0 0 0 0 0 1 0 "Biology" 19 2 2 1 0 1 0 0 0 0 1 6 1 2 1 1 2 3 0 0 0 0 0 0 0 4 4 2 3 3 3 3 3 1 2 2 2 101 101 99 2 3 2 1 1 1 2 1 2 0 0 4 4 4 3 3 3 3 3 4 2 5 4 4 1 1 0 21 2 2 3 3 4 3 3 3 1 0 1)

  ,(Ans 6 1 4 101 101 4 4 4 4 4 4 1 1 1 3 101 3 1 3 2 101 4 3 101 1 0 0 0 0 0 4 1 0 99 0 0 0 0 1 0 "Accounting" 16 1 101 1 0 1 0 1 0 0 1 3 1 3 1 1 1 2 0 0 0 0 0 0 0 4 3 4 3 2 2 101 101 101 101 101 101 4 7 6 5 5 4 1 3 5 5 0 0 3 5 3 3 2 3 4 2 3 2 3 4 3 3 3 3 3 1 21 1 4 5 3 101 101 2 3 0 0 0)

  ,(Ans 6 5 4 1 4 3 3 2 2 4 4 3 3 4 3 2 3 3 4 3 1 2 2 101 3 1 0 0 0 0 4 1 1 7 0 1 1 1 0 0 "Art" 13 1 101 1 0 0 0 0 0 0 1 2 2 2 101 2 2 2 1 0 0 0 1 0 0 1 2 2 2 2 2 2 2 2 3 2 1 1 10 4 1 2 2 2 2 2 1 1 1 1 3 1 101 2 3 3 3 1 1 1 2 2 4 3 3 3 1 21 2 2 3 3 4 4 2 1 3 3 3)

  ,(Ans 6 2 4 1 4 3 3 4 2 4 3 4 4 4 4 4 4 1 4 1 1 4 2 2 1 0 0 0 0 1 5 1 1 0 0 0 0 0 0 1 "Philosophy" 15 1 99 1 0 1 1 1 0 0 1 6 1 1 2 1 3 1 0 0 0 0 0 0 0 3 2 3 2 3 3 2 1 2 1 2 2 25 60 5 5 5 5 3 3 4 4 5 5 5 5 5 5 3 3 3 5 4 5 5 4 5 3 4 4 5 0 21 1 5 5 3 4 4 2 4 3 3 3)

  ,(Ans 6 4 4 4 4 4 2 4 4 4 2 1 3 4 3 4 4 4 4 4 3 3 1 3 4 0 1 0 0 0 3 1 1 0 1 0 0 0 0 0 "Accounting" 12 1 99 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 4 2 1 3 4 3 4 2 1 3 101 101 5 3 3 3 2 4 5 5 3 3 3 3 2 2 3 4 4 5 5 3 3 3 3 4 4 4 4 0 21 1 1 2 2 3 3 3 2 0 0 0)

  ,(Ans 6 2 4 3 3 3 3 3 3 3 2 3 3 2 2 2 2 2 2 2 2 3 3 2 2 0 0 0 0 1 5 0 0 0 0 0 0 0 0 1 "Philosophy" 16 1 101 1 0 1 1 0 0 0 1 1 2 1 2 2 2 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 2 10 5 5 5 5 3 2 3 2 2 2 4 3 4 4 3 4 4 4 4 4 4 4 4 4 4 3 3 0 21 1 4 5 3 3 4 3 3 1 1 1)

  ,(Ans 6 101 4 1 4 4 4 2 1 4 4 3 4 3 4 1 1 1 4 4 4 1 1 1 1 0 1 0 0 0 4 1 1 7 0 0 1 0 0 0 "Business Administration" 12 6 2 1 0 0 0 0 0 0 1 1 1 3 1 2 1 2 0 0 0 0 1 0 0 4 2 3 4 3 1 4 1 3 1 1 1 10 15 5 0 0 0 0 5 0 5 2 5 5 0 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 0 21 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 0 0 0 0 1 2 0 0 0 1 0 0 0 0 0 "Foreign Languages" 21 6 2 1 0 0 0 0 0 0 1 1 101 3 101 101 101 1 1 0 0 0 0 0 0 101 101 101 1 1 1 4 4 4 4 101 101 20 45 5 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 1 20 1 5 5 3 3 3 3 3 2 2 2)

  ,(Ans 6 6 4 4 4 3 4 1 1 4 4 4 4 4 4 4 1 1 1 1 2 1 3 2 2 0 0 0 1 0 5 0 0 0 0 0 0 0 0 1 "Business Administration" 12 6 2 1 0 0 0 0 0 0 1 1 101 101 2 101 101 1 0 0 0 0 0 0 0 101 101 101 101 1 1 1 1 1 1 3 1 15 25 5 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 3 4 4 4 4 5 5 3 3 1 20 2 3 3 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 5 1 1 4 1 0 0 0 0 0 "Electrical Engineering" 16 6 2 1 1 1 1 1 0 0 1 1 1 3 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 15 20 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 0 20 1 3 2 3 3 3 3 3 3 0 0)

  ,(Ans 6 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 1 0 0 0 4 1 1 7 0 0 1 0 0 0 "Civil Engineering" 15 6 2 1 0 1 1 1 0 0 1 6 2 3 1 1 2 1 0 1 0 1 0 1 0 3 3 3 3 3 3 3 3 3 3 3 2 35 60 3 3 5 4 3 5 5 5 4 4 4 4 3 3 3 4 4 5 4 3 4 4 4 4 4 4 4 1 20 1 3 2 3 3 3 3 2 2 2 2)

  ,(Ans 6 3 4 3 4 4 4 3 3 4 4 2 4 3 1 2 3 1 3 1 3 1 1 2 2 1 1 0 0 0 5 0 0 0 0 0 0 0 0 0 "Philosophy" 12 6 2 1 1 1 0 0 0 0 1 2 2 1 1 2 3 2 1 0 0 0 0 0 0 4 3 4 2 4 4 3 2 3 1 3 3 5 35 5 0 5 5 5 5 5 5 5 5 5 5 2 5 4 5 5 5 5 4 5 3 5 5 5 4 5 1 20 2 99 99 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 4 1 1 4 4 4 4 4 4 4 4 4 4 4 2 4 2 2 2 4 4 3 4 0 1 0 0 0 5 0 0 0 0 0 0 0 1 0 "Business Administration" 12 6 2 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 1 0 0 3 2 2 2 2 1 101 101 101 101 101 101 101 101 4 2 4 4 4 4 4 5 4 4 0 3 2 3 3 3 3 3 3 3 3 3 3 3 4 3 4 0 20 1 2 2 3 2 2 3 3 101 101 101)

  ,(Ans 6 3 3 1 1 3 4 3 2 2 101 3 3 3 3 1 1 1 1 1 3 1 1 2 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "English" 12 6 3 0 0 1 1 0 0 0 1 6 3 101 101 3 3 2 1 0 0 0 0 1 0 4 3 3 3 4 3 2 2 2 1 3 3 6 35 99 5 5 5 5 3 3 3 5 5 5 5 4 5 5 4 3 3 3 5 5 5 4 4 5 4 4 1 20 1 5 4 2 4 2 3 3 3 3 3)

  ,(Ans 6 3 3 2 4 4 4 3 2 3 3 2 3 4 3 3 4 1 4 2 3 2 2 3 4 0 1 0 1 0 4 1 1 7 0 0 1 0 0 0 "Anthropology" 14 5 2 1 0 1 1 1 0 0 1 2 2 3 1 1 2 2 1 0 0 0 0 0 0 4 4 4 3 3 4 4 3 4 2 3 4 20 25 5 5 5 5 3 5 4 5 5 4 4 5 2 2 2 2 2 2 2 2 2 2 2 4 5 4 4 1 20 2 2 3 3 3 2 2 2 3 3 3)

  ,(Ans 6 5 3 2 4 3 3 2 2 3 4 2 4 3 2 1 3 1 3 2 2 3 3 2 3 1 0 0 0 0 4 1 1 4 0 0 0 0 0 0 "Communication Studies" 16 5 101 1 0 1 1 0 0 0 1 6 3 1 1 1 2 2 1 0 0 0 0 0 0 3 3 3 2 3 3 2 2 2 1 2 3 0 5 4 5 5 5 2 3 2 3 1 5 1 5 4 4 4 3 3 2 4 3 5 2 4 3 4 4 4 1 20 1 2 3 2 4 3 3 3 1 2 0)

  ,(Ans 6 3 4 3 4 4 4 4 1 4 1 2 1 4 1 1 4 1 4 1 1 1 1 1 1 0 0 0 0 0 5 0 0 0 0 0 0 0 0 0 "Business Administration" 25 5 2 1 1 1 0 0 0 0 1 2 3 2 1 1 1 3 0 0 0 0 0 0 0 4 4 4 4 4 2 4 4 4 4 4 1 101 101 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 3 2 5 1 20 2 2 2 3 4 3 3 3 3 3 3)

  ,(Ans 3 5 4 4 4 4 4 3 1 4 4 3 4 4 4 1 3 2 1 3 3 2 1 3 1 1 0 0 0 0 2 0 1 7 0 0 0 0 0 0 "Undecided" 8 5 2 1 1 1 1 1 0 0 1 3 3 1 1 3 3 2 1 0 1 0 1 0 0 3 4 3 2 4 3 3 4 3 2 4 3 5 30 3 1 5 3 1 5 2 5 1 4 5 3 1 3 2 5 5 2 4 2 3 3 3 3 4 3 4 1 20 2 3 4 3 4 3 3 3 0 2 0)

  ,(Ans 3 4 3 1 1 4 2 4 4 1 2 1 4 3 1 1 3 2 3 3 4 2 1 2 1 0 0 0 1 0 4 1 1 99 0 0 0 0 0 1 "Psychology" 13 5 3 1 1 1 0 1 0 0 1 1 3 1 1 1 1 2 1 1 0 0 0 0 0 4 4 3 4 4 3 2 1 1 2 1 2 0 4 1 5 5 5 5 5 2 5 2 5 5 1 5 5 5 5 5 3 4 4 4 5 5 5 5 5 5 1 20 2 5 5 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 2 1 4 2 1 1 4 3 4 2 3 1 1 1 3 3 2 2 1 1 3 4 0 0 0 1 0 4 1 1 7 0 0 0 0 0 1 "Business Administration" 16 5 2 1 0 1 1 0 0 0 1 6 3 101 101 101 101 3 1 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 4 4 4 5 5 5 5 5 4 5 3 4 4 2 5 5 5 5 2 4 4 4 4 5 3 4 1 20 1 3 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 2 2 4 4 4 4 4 2 1 3 1 3 3 3 3 4 4 3 1 0 0 0 0 4 1 1 4 0 1 0 0 0 0 "Art" 6 5 2 1 0 0 0 0 0 0 1 3 3 1 1 2 3 2 1 0 0 0 0 0 0 4 4 3 3 3 4 4 4 4 3 3 4 15 30 5 1 4 3 2 3 0 0 1 1 1 1 3 1 2 4 4 3 3 3 3 4 4 4 4 3 3 0 20 2 5 2 3 4 3 3 3 3 2 3)

  ,(Ans 3 5 4 1 4 4 4 3 2 4 2 3 4 3 1 2 4 1 3 3 2 1 2 4 2 1 0 0 0 0 3 0 1 0 0 0 0 1 0 0 "Anthropology" 12 5 2 1 0 0 0 0 0 0 1 2 2 1 1 3 3 1 0 0 0 0 0 0 0 4 3 4 3 3 4 4 2 4 2 2 4 15 40 5 4 4 4 0 2 0 1 0 3 0 2 2 3 4 5 5 3 2 3 4 3 4 4 4 3 3 0 20 2 3 4 3 4 3 3 3 1 0 1)

  ,(Ans 6 2 4 1 4 3 3 4 1 3 2 4 2 3 2 2 2 2 3 2 3 1 2 3 3 0 0 0 0 1 4 1 1 0 0 0 0 0 0 1 "Biology" 16 4 2 1 0 1 1 1 0 0 1 1 3 1 1 1 3 2 0 0 0 0 1 0 0 4 2 2 2 3 3 3 1 1 1 2 1 15 50 5 0 0 0 3 5 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 4 3 4 1 20 1 2 2 1 4 3 3 3 1 1 1)

  ,(Ans 6 5 4 1 4 4 4 3 2 3 3 3 3 3 1 2 2 1 4 2 1 1 3 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Undecided" 14 4 2 1 0 0 0 0 0 0 1 1 1 1 1 1 3 2 0 0 0 0 0 0 0 4 4 4 3 3 4 3 2 2 2 1 3 15 45 5 5 5 5 5 3 1 3 0 5 3 5 3 3 4 3 3 3 1 1 5 2 2 3 4 4 4 0 20 2 3 4 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 4 3 3 2 3 3 4 4 3 3 4 2 1 3 1 2 2 3 3 4 0 0 0 0 1 3 1 0 7 0 0 0 0 0 1 "Mechanical Engineering" 15 4 99 1 0 1 1 1 0 0 1 2 2 1 1 1 3 2 1 0 0 0 0 0 0 3 4 2 1 3 4 3 4 2 2 3 3 20 45 5 0 0 0 2 5 0 1 0 0 2 0 4 4 3 4 3 3 2 5 5 3 4 4 3 2 4 0 20 2 2 4 3 4 3 3 2 3 3 3)

  ,(Ans 6 3 4 3 4 3 4 4 3 4 3 4 4 3 3 4 3 2 3 3 3 3 4 4 4 0 0 0 1 0 3 0 0 0 1 0 0 0 1 0 "Health Studies" 30 4 2 1 0 1 0 0 0 0 1 3 101 3 2 101 101 1 1 0 0 0 0 0 0 101 101 101 101 101 101 3 3 3 2 2 1 7 20 5 4 4 4 3 3 4 4 3 4 3 4 3 4 4 4 4 101 2 4 3 2 3 4 3 2 3 0 20 2 2 4 3 3 2 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 2 4 4 3 1 2 3 1 4 3 1 3 2 3 4 3 1 1 1 1 0 0 0 1 1 1 0 0 0 1 0 0 0 "Did not answer" 12 4 2 1 0 1 0 0 0 0 1 1 1 3 1 1 2 1 0 0 0 0 0 0 0 3 3 2 1 2 3 2 1 1 1 2 2 20 50 5 0 0 0 0 3 2 2 0 0 0 0 1 3 1 5 5 5 2 1 2 3 3 2 4 99 99 0 20 2 99 2 3 4 3 3 3 3 2 3)

  ,(Ans 6 4 4 2 4 4 4 4 3 4 4 4 2 2 3 1 2 1 3 1 2 1 1 3 1 0 1 0 0 0 4 1 1 0 0 1 0 1 0 0 "English" 16 4 1 1 0 1 1 1 0 0 1 2 2 1 2 3 2 2 0 1 0 0 0 1 0 4 3 2 1 3 2 4 3 2 1 3 2 2 5 99 5 5 5 2 5 4 5 4 4 5 5 4 5 3 5 5 3 5 4 4 5 5 5 5 5 5 1 20 2 3 4 3 3 2 3 3 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 3 4 4 2 2 3 3 3 1 1 1 1 1 2 2 1 3 1 0 1 0 0 0 4 0 1 0 0 0 1 0 0 0 "Communication Studies" 14 4 2 0 0 1 0 0 0 0 1 1 3 1 1 1 3 99 0 1 0 0 0 0 0 4 3 3 3 4 4 4 2 2 2 4 4 0 8 99 5 5 5 5 5 5 3 2 5 5 5 3 2 3 5 5 4 4 2 3 4 2 4 5 2 4 1 20 2 4 5 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 4 3 2 4 4 4 2 2 3 3 4 4 3 1 2 1 4 1 3 1 4 3 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Music" 15 4 3 1 0 0 0 0 0 0 1 3 1 1 2 3 3 2 1 1 0 0 0 1 0 4 3 4 3 3 4 3 2 4 1 3 3 4 30 5 0 0 0 3 5 5 5 5 5 5 2 2 3 2 101 5 3 4 5 5 4 3 4 5 4 5 1 20 2 5 3 3 3 2 3 3 1 2 1)

  ,(Ans 6 5 3 2 2 3 3 3 2 2 3 2 2 2 2 1 2 1 3 2 2 1 1 4 3 1 0 0 0 0 4 0 1 7 0 0 0 0 0 1 "Physics" 16 4 2 1 1 0 0 0 0 0 1 2 2 1 2 1 3 2 0 0 0 0 1 0 0 3 3 3 3 3 101 101 101 101 101 101 1 5 20 5 2 5 2 2 1 1 3 2 3 2 3 3 3 3 3 3 3 2 3 3 3 3 2 3 2 2 1 20 1 3 4 3 4 3 3 3 1 2 1)

  ,(Ans 6 3 4 1 4 3 3 2 2 3 3 2 4 3 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 4 1 1 0 0 1 0 1 0 0 "Electrical Engineering" 12 4 1 1 0 1 0 0 0 0 1 2 3 1 2 1 3 99 0 0 0 0 0 0 0 4 4 3 2 3 2 3 1 1 1 2 1 10 50 6 3 1 0 0 2 1 4 2 1 0 0 3 2 2 4 4 4 5 3 3 2 2 4 4 3 4 0 20 1 5 5 3 3 3 3 2 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 1 3 4 4 4 4 4 3 4 4 1 4 1 1 2 2 4 3 1 0 0 0 0 5 1 1 7 0 0 0 0 0 0 "Art" 13 4 2 0 0 1 1 1 0 0 3 2 1 2 2 1 2 2 1 0 0 0 0 0 0 4 4 4 1 2 4 4 3 4 1 1 3 15 45 5 5 5 5 5 5 3 2 2 4 2 5 4 4 4 5 5 5 2 3 4 3 5 5 5 5 5 1 20 2 3 2 3 3 3 3 3 0 0 1)

  ,(Ans 6 5 3 3 3 4 4 4 3 3 4 3 3 3 3 3 3 3 3 3 3 2 3 3 3 1 0 0 0 0 4 1 1 3 0 0 0 0 0 1 "Undecided" 10 4 2 1 0 1 1 1 0 0 3 1 2 2 1 1 2 2 0 0 0 0 0 0 0 3 3 3 3 2 3 3 3 3 3 2 3 20 60 5 3 3 3 0 2 1 1 2 2 2 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 20 1 1 1 3 3 3 3 3 3 0 0)

  ,(Ans 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 1 0 0 0 0 4 1 1 4 0 0 1 0 0 0 "Business Administration" 12 4 2 1 0 1 0 1 0 0 1 1 1 2 2 1 3 2 1 0 0 0 0 0 0 4 4 4 3 3 3 3 101 3 3 3 3 15 50 5 3 5 4 4 4 5 5 5 4 3 2 2 3 2 5 5 4 5 4 3 3 3 3 4 4 4 0 20 1 2 2 3 4 2 2 2 1 0 1)

  ,(Ans 6 5 3 1 4 2 2 1 2 3 2 2 2 3 3 1 2 1 2 1 1 1 1 1 1 0 0 0 0 0 4 0 1 7 0 1 0 0 0 0 "Political Science" 15 4 2 1 1 1 0 1 0 0 1 1 1 3 2 1 1 2 1 0 0 0 0 0 0 4 3 3 1 4 3 2 1 2 1 101 3 30 45 5 5 3 4 101 0 0 2 0 5 2 5 3 3 3 5 3 3 4 3 5 2 5 1 4 3 2 1 20 1 4 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 2 1 4 3 4 4 4 3 1 2 1 4 4 4 1 1 4 4 1 0 0 0 0 2 0 1 7 1 1 1 0 0 0 "Undecided" 14 4 99 1 0 0 0 0 0 0 2 6 1 1 2 1 3 2 0 0 0 0 0 0 0 3 4 3 3 4 3 3 4 3 3 4 2 6 44 3 1 1 1 0 2 2 2 0 1 1 2 2 1 1 5 4 1 1 1 1 4 1 4 4 3 3 0 20 2 3 2 3 4 4 3 3 0 0 1)

  ,(Ans 3 6 2 1 3 4 3 1 3 4 4 1 1 1 2 3 1 4 1 1 1 1 1 1 1 0 1 0 0 0 3 1 1 7 0 0 0 0 0 0 "Philosophy" 12 3 2 1 0 0 0 0 0 0 1 2 1 3 1 2 1 2 0 0 0 0 0 0 0 101 101 101 101 101 101 101 101 101 101 101 101 2 5 4 5 5 5 5 5 2 3 3 5 5 5 3 3 2 4 5 3 4 3 4 3 3 3 4 3 3 0 20 1 2 2 3 4 4 3 3 0 0 0)

  ,(Ans 6 3 3 1 3 4 3 2 2 3 4 2 3 4 3 2 4 1 4 4 1 3 1 4 3 1 0 0 0 0 5 0 0 7 1 0 1 0 0 0 "Mechanical Engineering" 12 3 101 1 0 1 0 0 0 0 1 1 3 1 1 1 3 1 1 0 0 0 1 1 0 4 3 3 1 4 3 4 3 3 1 4 4 20 60 6 5 4 5 5 4 3 0 2 2 0 5 2 2 2 1 1 3 1 2 2 2 4 5 5 3 4 1 20 1 2 2 3 4 4 3 3 0 0 0)

  ,(Ans 3 3 3 1 4 4 4 2 1 4 4 2 3 3 4 4 2 1 2 1 3 4 4 4 2 0 1 0 0 0 4 1 1 7 0 0 1 0 0 0 "Foreign Languages" 15 3 2 1 0 1 1 1 0 0 1 2 2 1 1 3 3 2 1 0 0 0 1 0 0 4 3 3 3 4 3 4 3 3 2 4 3 2 30 5 0 0 0 1 0 0 0 0 0 0 0 3 5 4 4 4 2 3 3 3 1 3 2 3 3 3 0 20 1 2 3 3 4 2 3 2 0 0 3)

  ,(Ans 6 5 4 3 4 4 4 4 3 3 3 3 3 3 3 1 2 1 3 1 1 4 4 4 4 0 0 0 1 0 4 0 1 7 0 0 1 0 0 0 "English" 12 3 1 0 1 1 1 1 0 0 1 2 2 1 1 1 3 2 1 0 0 0 0 0 0 3 3 3 3 101 4 2 3 1 101 101 1 20 15 6 5 5 5 5 1 5 1 1 5 5 5 2 3 3 101 101 101 101 101 101 101 101 101 101 101 101 0 20 2 3 3 3 3 2 3 3 1 0 0)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 3 3 4 3 4 4 2 3 1 2 3 3 4 4 0 1 0 0 0 3 0 1 7 1 0 0 0 0 0 "Social Work" 17 3 99 1 0 0 0 0 0 0 1 3 3 1 1 1 3 2 1 1 0 0 0 0 0 4 4 4 3 2 2 4 4 4 3 3 3 25 60 6 5 5 4 5 4 4 4 4 101 4 4 3 3 2 2 2 2 2 2 2 101 2 5 5 5 5 1 20 2 1 1 3 3 2 3 3 0 0 0)

  ,(Ans 5 5 2 1 3 4 4 1 1 1 4 4 4 1 3 1 2 1 1 1 3 1 2 4 4 1 0 0 0 0 2 1 1 7 0 0 0 0 0 1 "Undecided" 15 3 99 1 0 0 0 0 0 0 1 6 3 1 1 3 1 3 0 1 0 0 0 1 0 3 2 2 1 3 2 3 1 1 1 2 2 101 101 5 4 4 3 5 5 101 5 2 4 5 4 4 2 2 4 4 2 4 4 4 4 3 5 4 3 3 1 20 1 3 1 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 4 3 3 3 4 1 3 3 4 4 4 4 4 2 3 2 3 2 3 4 4 3 2 1 0 0 0 0 3 1 1 0 1 0 1 0 0 0 "Foreign Languages" 15 3 101 1 0 1 0 0 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 0 1 4 4 3 3 3 3 101 101 101 101 101 101 101 101 4 4 4 4 2 3 4 5 5 5 4 5 1 2 2 2 2 3 3 2 2 2 2 99 3 99 2 0 20 2 4 4 3 3 3 3 3 0 0 0)

  ,(Ans 6 5 4 1 1 3 4 4 4 2 3 4 3 4 4 3 3 3 3 2 1 3 3 3 4 1 1 0 0 0 4 0 1 0 0 0 1 1 0 0 "Architecture" 12 3 101 0 0 1 0 0 0 0 1 1 1 1 1 2 3 2 1 0 0 0 1 0 1 4 4 3 3 3 3 101 101 101 101 101 101 101 10 6 0 3 3 4 5 5 5 4 4 3 3 2 3 2 3 4 3 3 2 2 3 3 4 4 4 4 1 20 2 5 5 3 4 4 2 4 0 0 3)

  ,(Ans 6 1 4 2 4 4 4 4 3 4 2 4 4 3 3 1 2 1 3 1 2 1 1 4 2 1 0 0 0 0 4 0 0 0 1 1 0 0 0 0 "Undecided" 13 3 2 1 0 0 0 1 0 0 1 2 2 2 101 1 3 2 1 1 0 0 0 0 0 3 3 4 3 101 2 4 3 4 3 4 3 3 13 5 2 2 1 2 3 2 1 2 2 1 1 2 2 2 4 3 3 2 1 2 2 2 4 5 3 4 1 20 1 4 4 3 4 4 3 3 0 0 0)

  ,(Ans 6 3 4 4 4 3 3 4 2 4 4 1 4 4 1 1 4 1 4 3 3 1 4 3 3 0 0 0 1 0 4 0 1 0 1 0 0 0 0 0 "Business Administration" 14 3 2 1 0 1 1 1 0 0 1 1 3 1 1 1 2 2 1 0 0 0 0 1 0 2 3 3 3 3 2 2 3 4 2 2 4 1 10 5 5 5 5 5 5 2 2 5 5 5 5 4 3 3 4 3 3 4 4 4 3 3 4 4 2 4 0 20 1 4 3 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 4 4 4 4 4 3 2 3 2 4 4 3 3 3 3 3 2 2 1 1 1 2 3 1 0 1 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 13 3 2 1 1 1 1 1 0 0 1 2 2 1 1 1 3 1 0 1 0 0 0 0 0 3 3 2 1 2 2 3 2 2 1 2 3 20 45 4 2 1 1 0 2 2 2 1 2 0 2 2 2 2 4 5 4 3 2 3 1 4 5 5 99 99 0 20 2 3 3 3 3 3 3 2 1 1 1)

  ,(Ans 6 3 4 1 3 4 4 3 2 2 3 3 4 3 1 1 1 1 1 1 2 1 2 3 3 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Art" 17 3 2 1 0 1 0 1 0 0 1 6 101 2 101 3 2 2 1 1 0 0 0 0 0 4 4 4 3 4 4 3 3 4 2 2 3 5 30 99 0 0 1 5 5 4 5 5 5 3 0 2 3 3 5 5 4 4 4 4 4 5 5 5 3 5 99 20 2 2 2 3 4 3 3 3 1 1 1)

  ,(Ans 6 4 4 1 3 4 3 3 4 4 4 4 4 3 1 4 3 1 4 3 2 3 3 2 4 0 0 0 1 0 4 1 1 7 0 0 0 1 0 0 "Art" 15 3 2 1 0 0 0 0 0 0 3 6 3 1 1 1 3 1 1 0 0 0 0 0 0 4 3 4 1 3 4 3 1 4 1 3 4 10 60 5 5 5 5 3 3 5 5 4 5 5 5 5 5 101 2 2 4 5 5 5 2 5 5 5 4 5 0 20 1 4 5 3 4 4 4 3 1 1 1)

  ,(Ans 6 5 3 1 4 4 4 2 4 4 3 4 3 3 2 1 3 1 3 3 3 3 3 4 4 1 0 1 0 0 4 1 1 0 1 1 1 1 0 1 "Art" 12 3 101 1 0 1 1 0 0 0 1 6 1 1 2 1 3 2 1 0 0 0 0 0 0 3 3 3 3 4 3 3 3 3 3 4 3 2 20 4 5 5 2 0 2 1 1 0 2 0 0 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 0 20 1 3 2 3 4 4 3 3 1 0 1)

  ,(Ans 6 5 4 1 1 4 4 3 3 3 2 3 4 3 3 2 2 1 3 3 3 3 3 3 1 0 1 0 0 0 4 1 0 7 0 0 0 0 1 1 "Communication Studies" 16 3 2 1 0 1 0 1 0 0 1 1 101 101 101 101 3 2 0 1 0 0 0 0 0 4 3 3 2 3 4 3 4 1 1 2 4 3 30 3 5 5 5 5 5 5 5 3 5 4 5 5 5 5 5 5 4 3 2 4 4 5 5 5 3 3 0 20 2 2 4 3 3 2 3 3 3 3 3)

  ,(Ans 6 1 4 4 4 4 4 3 3 4 2 4 3 4 4 3 4 4 4 4 4 4 2 2 3 1 0 0 0 0 4 1 1 7 1 1 1 0 1 0 "Business Administration" 17 3 2 1 1 1 1 0 0 0 1 3 3 1 1 1 3 1 1 0 0 0 0 0 0 4 3 4 3 3 3 4 4 4 4 4 4 20 35 6 0 2 1 1 4 5 5 3 3 3 3 1 2 2 2 3 3 3 3 3 3 3 3 3 3 3 0 20 2 2 4 2 2 2 2 2 2 2 2)

  ,(Ans 6 3 4 1 1 4 4 3 2 2 4 2 3 3 2 1 1 1 1 1 4 1 1 4 1 0 0 0 1 0 4 1 1 99 0 0 0 0 0 1 "Environmental Sciences and Resources" 16 3 2 1 0 1 1 0 0 0 1 1 3 1 1 2 1 2 0 1 0 0 1 1 0 4 3 3 2 3 2 4 4 2 1 2 3 1 4 5 5 5 5 4 3 3 5 2 5 3 4 5 4 5 5 5 4 4 3 5 4 5 4 4 4 3 1 20 1 4 5 3 3 3 3 3 1 1 1)

  ,(Ans 5 2 3 1 4 3 2 4 1 4 4 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 4 1 1 1 0 0 0 0 0 1 "Political Science" 13 3 101 1 0 1 1 1 0 0 1 1 3 1 2 1 2 99 1 0 0 1 0 0 0 4 2 2 2 2 4 4 2 1 2 2 4 1 10 1 5 5 5 5 3 3 5 5 5 5 5 4 3 5 3 4 3 5 5 5 5 5 1 1 1 1 0 20 1 5 5 3 3 4 3 3 0 0 0)

  ,(Ans 6 5 4 3 3 4 4 4 2 3 3 3 3 3 2 4 3 3 3 2 3 3 3 2 2 0 0 0 0 1 5 1 1 0 0 0 0 0 0 1 "Civil Engineering" 17 2 2 1 0 1 1 0 0 0 1 1 1 2 1 1 2 1 1 0 0 0 0 0 0 1 3 1 1 1 1 1 3 1 1 1 1 5 15 6 3 4 4 3 3 3 3 3 3 4 3 3 4 3 4 4 3 3 4 4 4 4 4 4 3 3 0 20 1 4 4 3 4 4 3 3 1 2 1)

  ,(Ans 6 5 3 1 1 2 3 2 3 4 2 3 3 2 3 3 3 2 3 1 1 3 2 2 1 0 0 0 1 0 4 1 0 0 0 0 0 0 0 0 "Business Administration" 14 2 2 1 0 1 0 0 0 0 1 1 101 3 101 101 101 1 0 0 0 0 0 0 0 4 3 3 2 3 3 3 3 3 1 2 2 7 20 6 5 5 5 5 1 0 5 3 5 3 5 3 4 4 4 4 2 4 2 4 3 4 4 4 3 3 1 20 1 4 4 3 4 2 2 2 3 3 3)

  ,(Ans 6 2 4 1 2 4 4 4 3 2 2 3 3 3 4 1 1 1 1 1 1 1 1 1 1 0 0 0 1 0 4 0 1 0 0 0 0 0 1 0 "Biology" 14 2 101 1 0 1 1 1 0 0 1 2 1 1 1 1 3 2 1 1 0 0 0 0 0 4 4 4 2 3 3 4 4 3 1 2 2 7 35 5 3 3 3 0 3 2 5 0 3 1 5 2 4 3 2 2 3 4 3 4 5 4 5 5 4 3 1 20 2 5 5 3 3 4 2 3 3 3 3)

  ,(Ans 6 4 4 1 3 4 4 4 2 2 3 4 3 4 2 1 3 1 4 3 4 3 1 4 3 1 0 0 0 0 3 1 1 0 0 0 0 1 1 0 "Psychology" 16 2 101 1 0 0 0 0 0 0 1 1 3 1 1 1 2 3 1 1 0 0 1 1 1 4 4 4 3 4 4 4 3 4 3 4 4 101 101 5 5 5 5 3 3 5 5 2 3 2 1 4 4 3 4 4 3 3 2 3 3 4 4 4 4 3 1 20 2 4 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 3 4 3 3 2 4 4 4 1 1 3 2 1 2 3 3 3 3 4 4 3 2 0 0 0 0 1 3 1 1 0 0 1 1 1 0 0 "Electrical Engineering" 16 2 1 1 0 1 0 0 0 0 1 3 3 3 2 1 2 2 1 1 0 0 0 1 0 4 3 3 3 4 2 4 4 4 3 4 3 1 7 6 2 1 0 0 0 0 0 0 0 0 0 4 3 2 2 4 3 1 3 3 3 3 3 2 2 99 0 20 1 3 101 3 2 2 3 3 0 0 0)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 1 3 1 4 3 1 1 1 4 1 2 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "History" 17 2 2 1 0 1 0 1 0 0 1 2 3 1 1 1 3 2 1 0 0 0 1 0 0 4 3 3 3 3 4 4 3 2 2 2 4 20 75 4 5 5 4 3 2 2 2 3 5 0 5 4 5 1 3 4 4 2 4 5 2 4 3 4 99 99 0 20 2 3 4 3 3 3 3 3 0 0 3)

  ,(Ans 6 3 4 1 4 4 3 3 1 2 3 4 2 3 1 1 3 1 2 1 3 1 1 2 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Music" 12 2 99 1 0 0 0 0 0 0 1 6 1 1 2 2 3 2 0 1 0 0 0 1 0 4 3 4 4 3 4 4 3 3 101 3 4 5 20 99 1 1 1 3 4 0 1 1 1 0 4 3 3 3 101 5 3 4 2 3 3 3 5 5 4 3 1 20 2 3 3 3 3 2 3 3 3 3 3)

  ,(Ans 6 2 4 4 4 4 4 4 3 4 4 4 4 4 4 3 4 2 3 2 2 2 3 2 4 1 0 0 1 0 4 1 1 99 0 0 0 0 0 1 "Did not answer" 20 2 2 1 1 1 0 1 0 0 1 2 3 1 3 1 3 1 1 1 0 0 0 0 0 3 4 3 3 3 3 3 4 3 3 3 3 101 45 4 3 5 5 2 5 2 5 5 5 2 3 4 5 5 5 5 4 4 4 4 4 4 4 5 4 5 0 20 1 3 2 3 4 3 3 3 0 0 0)

  ,(Ans 5 5 2 1 4 3 2 4 3 4 4 3 4 3 1 1 2 1 3 1 3 3 3 2 2 1 0 0 0 0 3 0 1 0 0 0 1 0 0 0 "Psychology" 12 2 2 1 0 1 0 1 0 0 1 1 3 1 1 2 1 99 0 0 0 1 0 1 0 3 3 3 2 3 3 4 3 3 2 3 3 0 5 5 3 3 4 5 5 3 3 2 4 3 4 3 2 3 4 4 4 3 2 3 4 3 4 5 3 5 1 20 1 4 5 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 1 3 4 4 4 4 4 4 4 4 2 3 3 4 3 2 4 3 4 3 4 4 4 1 0 0 0 0 4 1 1 0 1 1 1 0 0 0 "Business Administration" 15 2 1 1 0 1 1 0 0 0 3 3 1 1 1 1 3 1 1 1 0 0 0 0 0 3 4 3 3 3 3 3 4 3 3 3 3 30 50 6 5 5 3 5 3 3 4 4 4 5 5 2 1 2 5 5 1 1 2 3 4 3 5 5 4 5 1 20 1 1 1 1 1 1 3 2 0 0 0)

  ,(Ans 6 5 4 1 3 4 4 4 1 4 4 3 4 3 3 1 3 1 3 3 4 1 2 3 2 1 1 0 0 0 4 0 1 7 0 0 1 1 0 0 "Psychology" 30 2 2 0 0 1 1 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 0 0 0 4 3 3 2 4 4 3 2 3 2 2 2 101 101 5 0 3 3 1 2 0 4 1 3 1 1 1 2 2 4 4 3 2 2 3 3 3 4 4 4 4 1 20 2 2 5 3 3 2 3 3 1 0 1)

  ,(Ans 6 5 4 1 3 4 4 2 4 4 1 2 3 2 3 2 2 1 2 3 3 1 1 3 1 0 0 0 0 1 3 1 1 0 0 0 0 0 0 1 "Physics" 15 2 2 1 0 1 0 0 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 0 0 3 3 4 4 4 4 2 2 4 2 3 3 101 101 6 1 0 1 2 1 0 0 2 1 0 0 3 3 3 3 3 3 3 3 3 2 2 3 3 2 1 0 20 2 4 5 3 4 4 2 2 1 1 1)

  ,(Ans 4 5 2 2 3 4 4 2 1 2 4 3 2 2 4 1 2 2 2 1 2 1 1 4 2 0 1 0 0 1 4 1 1 7 0 1 0 0 0 0 "Undecided" 12 2 2 1 0 0 0 0 0 0 1 1 3 1 1 1 2 3 1 0 0 0 0 0 0 4 3 3 3 4 4 4 3 2 2 3 3 101 101 5 3 3 2 5 5 2 2 2 4 4 5 1 2 2 2 3 2 1 2 3 4 3 4 3 2 1 0 20 1 3 99 3 4 1 3 3 3 2 3)

  ,(Ans 6 3 4 3 4 4 4 3 4 3 3 3 4 3 3 2 2 2 2 2 2 2 3 2 2 0 1 0 0 0 4 0 0 2 1 0 0 0 0 0 "Computer Engineering" 15 2 99 1 0 1 1 1 0 0 1 3 2 2 2 1 2 1 0 0 0 0 1 1 0 3 3 3 3 3 2 3 3 3 3 3 2 8 40 6 2 3 2 3 2 4 3 3 2 3 2 2 3 3 3 3 3 3 3 3 3 3 4 4 4 4 1 20 1 2 2 3 3 4 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 1 1 3 3 3 4 3 3 3 2 2 2 2 2 1 1 3 2 1 1 0 1 0 3 1 1 0 0 0 0 0 0 1 "Social Science" 13 2 2 1 0 1 1 1 0 0 1 2 1 1 1 3 3 2 1 0 0 0 0 0 0 4 3 4 2 4 4 4 1 4 2 4 4 4 15 6 2 5 5 5 3 3 3 3 5 3 5 4 4 4 4 4 4 4 4 4 3 4 3 4 4 4 1 20 1 3 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 3 1 4 4 3 4 3 3 3 2 4 4 3 1 3 1 3 3 2 3 3 4 3 0 1 0 0 0 4 1 1 7 0 0 0 0 1 0 "East Asian Studies" 18 2 2 1 0 1 1 0 0 0 1 2 3 3 2 1 3 2 1 1 0 0 0 0 0 3 3 3 2 4 2 3 2 2 2 3 2 15 60 5 0 5 0 5 2 0 0 0 4 1 1 1 3 3 4 2 2 1 3 4 3 3 3 4 3 3 0 20 2 3 2 3 4 2 3 3 0 3 0)

  ,(Ans 6 3 101 1 4 4 4 4 3 3 4 1 4 4 3 1 3 1 4 3 3 2 3 4 1 0 1 0 0 0 4 0 1 0 0 0 1 0 0 0 "Architecture" 15 2 101 1 0 1 0 0 0 0 1 3 3 1 1 1 1 2 0 0 0 0 0 0 0 2 1 3 2 3 1 2 1 3 2 3 1 1 5 3 5 5 3 0 3 2 4 4 4 3 5 3 3 2 2 2 2 1 2 3 3 3 4 4 4 4 0 20 1 3 3 4 1 4 4 4 0 0 0)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 1 4 4 4 1 4 4 4 4 4 0 1 0 0 0 4 1 0 99 0 0 0 0 0 0 "Business Administration" 12 1 101 1 0 1 0 0 0 0 1 3 1 2 1 1 3 2 0 0 0 0 0 1 0 1 101 101 101 101 101 3 3 4 4 4 4 10 15 6 0 5 5 1 2 1 5 5 5 0 5 1 2 5 5 3 3 4 2 2 3 3 5 5 5 5 1 20 2 4 5 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 4 4 4 3 2 3 1 4 3 3 4 4 3 4 2 4 3 3 4 1 4 3 4 0 1 0 0 0 1 1 1 0 0 0 0 0 1 0 "Accounting" 16 1 101 1 0 1 1 0 0 0 1 3 101 101 101 101 3 2 1 0 0 0 0 1 0 3 101 3 101 1 3 3 3 4 101 1 3 7 20 6 2 3 4 0 1 101 0 0 1 1 1 2 2 2 2 2 2 2 2 2 2 2 4 4 3 3 1 20 1 2 3 3 101 1 2 2 0 0 0)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 1 3 2 2 1 1 1 2 1 1 1 1 3 1 0 0 0 0 0 5 1 1 0 0 0 0 0 0 0 "English" 16 1 99 1 0 1 1 0 0 0 1 1 3 101 101 101 101 3 1 0 0 0 0 1 0 3 4 3 1 3 2 3 4 3 1 3 2 101 101 4 5 4 4 4 3 2 4 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 20 1 2 4 3 4 4 3 1 3 0 3)

  ,(Ans 6 4 101 4 4 4 4 3 4 3 4 3 4 4 4 3 4 3 4 4 3 4 2 2 2 1 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "Political Science" 17 1 99 1 0 0 0 0 0 0 1 3 3 1 1 1 2 3 0 0 0 0 0 1 0 101 101 101 101 101 101 4 4 4 4 3 3 101 101 99 5 5 5 2 1 1 5 2 2 1 5 3 3 3 2 3 3 3 2 3 2 2 3 3 2 4 1 20 1 4 2 3 3 2 2 3 0 0 0)

  ,(Ans 6 3 4 4 4 4 4 4 3 4 4 3 3 4 2 2 2 3 101 2 4 2 1 3 4 1 0 0 0 0 4 1 1 99 0 0 0 1 0 0 "Accounting" 16 1 101 1 0 1 0 0 0 0 1 3 2 1 1 1 3 1 1 0 0 0 0 0 0 4 3 4 3 2 3 4 3 3 3 2 3 150 45 5 2 0 2 3 0 0 0 0 0 3 0 1 1 1 1 1 2 1 2 1 3 1 5 5 3 2 1 20 1 1 1 3 3 4 3 4 2 2 2)

  ,(Ans 3 6 3 3 4 3 3 3 1 4 3 3 2 3 3 1 3 1 2 2 2 1 2 2 2 1 0 0 1 0 4 1 1 0 1 0 1 0 0 0 "Business Administration" 14 1 101 1 0 1 0 0 0 0 1 1 3 1 1 1 3 3 1 0 0 0 0 0 0 3 1 3 2 1 2 3 3 3 3 3 3 101 101 6 5 5 3 5 3 5 2 5 4 1 3 2 1 1 1 1 1 1 1 2 2 1 4 3 3 3 1 20 1 4 4 3 3 1 2 4 0 0 0)

  ,(Ans 6 5 2 1 3 4 4 1 2 3 4 2 2 2 2 2 2 1 2 2 1 1 2 1 2 1 0 0 0 0 3 0 0 0 0 1 0 0 0 0 "Social Work" 12 1 101 1 0 1 0 0 0 0 1 6 101 101 101 101 3 2 0 0 0 0 0 0 0 3 2 2 1 2 2 2 2 2 1 1 3 101 45 99 5 4 3 0 1 0 0 0 3 0 0 2 2 2 2 1 1 2 2 3 2 2 2 4 99 99 0 20 1 2 99 3 4 4 1 3 3 3 3)

  ,(Ans 6 3 4 4 4 4 3 4 3 4 2 3 2 4 2 1 2 2 4 3 3 3 2 1 1 0 0 0 0 1 4 1 1 2 1 0 0 0 0 1 "Accounting" 13 1 101 1 0 1 1 0 0 0 1 1 101 3 101 101 101 2 0 0 0 0 0 0 0 4 3 4 3 101 101 4 3 4 3 101 101 15 30 6 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 20 1 2 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 4 4 4 4 4 4 4 4 4 3 4 3 4 3 3 3 1 2 2 2 2 2 2 2 0 1 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 20 1 2 1 0 1 1 1 0 0 1 2 1 3 1 1 2 1 1 0 0 0 1 0 0 3 4 4 4 4 3 3 3 3 3 3 2 10 25 6 2 2 2 2 2 4 4 2 2 0 5 1 1 1 3 4 4 3 2 2 2 2 4 4 4 4 1 20 1 2 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 1 1 4 4 4 4 4 4 4 4 1 4 4 3 3 3 3 3 3 3 3 3 3 3 0 0 1 0 0 2 1 1 1 0 0 1 0 0 0 "Elementary Education" 13 1 101 1 0 1 1 1 0 0 1 3 3 3 1 2 1 2 0 0 0 0 1 0 0 101 101 101 101 101 101 4 4 3 3 3 3 11 25 3 2 2 3 3 1 4 2 1 2 2 2 2 3 4 4 4 3 4 3 3 3 3 3 3 3 3 0 20 1 1 1 4 4 4 4 4 3 0 3)

  ,(Ans 6 5 3 4 4 4 4 3 3 4 3 4 2 3 4 1 3 2 4 4 4 1 1 3 1 1 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Music" 13 1 101 1 0 0 0 0 0 0 1 1 3 1 1 2 2 3 1 0 0 0 0 0 0 4 4 4 4 3 2 101 101 101 101 101 101 101 101 99 3 4 101 2 5 1 0 0 1 0 3 1 1 1 2 2 1 1 1 1 1 2 3 3 4 4 1 20 2 5 5 3 4 4 3 3 0 0 0)

  ,(Ans 6 6 4 2 3 4 4 4 3 3 4 3 4 4 2 4 2 1 1 1 2 2 1 3 4 1 1 0 0 0 4 1 1 0 1 1 1 0 0 0 "Film" 9 1 2 1 0 1 1 0 0 0 3 2 3 101 101 101 3 1 0 1 0 0 0 0 0 4 4 3 3 2 4 4 3 3 2 1 4 101 60 5 2 4 5 2 2 1 4 0 3 1 3 1 1 2 3 4 1 1 1 2 1 3 4 3 3 3 1 20 2 2 5 3 4 3 3 1 3 2 3)

  ,(Ans 6 5 4 1 3 4 3 2 1 2 3 2 3 4 101 1 3 1 4 4 4 1 3 4 1 1 0 0 1 0 4 0 0 0 0 0 0 0 0 0 "Anthropology" 13 1 101 1 0 0 0 0 0 0 1 1 3 1 1 1 2 3 0 0 0 0 1 1 0 3 3 3 3 1 3 3 2 2 1 1 3 101 101 5 0 0 0 5 5 0 0 0 0 5 0 2 3 1 4 4 1 3 4 2 4 1 3 5 2 1 1 20 1 5 2 3 3 4 3 3 3 3 3)

  ,(Ans 3 6 4 4 4 4 4 3 3 4 4 1 3 3 3 3 3 3 3 3 3 3 3 3 3 0 1 0 0 0 4 1 0 101 0 0 0 0 0 0 "Electrical Engineering" 12 1 101 1 0 0 0 0 0 0 1 3 101 3 101 101 101 3 0 0 0 0 1 0 0 4 1 4 3 3 3 3 1 3 3 3 3 101 101 6 1 1 4 1 2 0 2 3 3 2 1 1 2 2 2 2 2 2 2 3 2 3 3 3 3 3 0 20 1 2 1 3 3 3 3 3 0 0 0)

  ,(Ans 6 6 4 4 4 3 3 3 1 3 2 1 2 4 2 4 2 4 4 1 2 2 3 2 4 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Accounting" 12 2 2 1 0 0 0 0 0 0 1 6 1 3 1 1 2 1 1 0 0 0 0 0 0 4 3 2 2 3 2 4 3 1 3 3 1 10 20 5 0 0 0 0 0 0 0 0 0 0 0 4 5 5 5 5 5 5 3 4 2 5 3 3 99 99 0 20 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 4 4 3 4 3 4 3 3 3 3 2 4 3 3 3 4 3 4 4 3 3 3 3 3 0 1 0 0 0 2 1 1 99 0 0 1 0 0 1 "Biology" 12 1 99 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 1 0 0 3 3 3 3 1 2 3 3 3 2 1 2 101 101 6 5 5 5 2 1 1 5 0 4 0 5 2 2 101 1 1 1 2 1 2 1 2 4 4 3 3 1 20 1 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 3 4 4 3 1 1 2 4 4 4 2 1 1 1 1 1 1 1 1 1 3 0 0 0 0 1 4 1 0 7 0 1 0 0 0 0 "History" 101 101 2 1 1 0 0 0 0 0 1 1 3 1 1 1 2 2 0 0 0 0 0 0 0 4 2 2 1 101 3 3 1 1 1 1 1 1 8 99 5 5 5 2 4 5 4 5 3 3 5 5 5 2 3 3 4 4 4 5 4 5 5 5 2 2 1 19 1 3 1 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 3 101 4 4 3 2 2 4 2 4 3 3 3 4 3 1 2 2 1 3 4 3 4 1 0 0 0 0 4 1 0 7 0 0 0 0 0 1 "Social Science" 8 6 2 1 1 1 1 0 0 0 1 3 1 3 1 1 1 1 0 0 0 0 0 0 0 3 1 1 1 1 3 3 1 1 1 1 3 15 40 5 0 5 4 0 3 1 1 0 3 1 1 1 2 1 4 4 3 2 1 2 3 2 3 3 1 1 1 19 2 3 2 3 4 3 3 2 0 0 1)

  ,(Ans 6 6 4 2 2 3 4 4 1 2 4 4 4 2 2 1 1 1 1 1 1 1 2 2 2 1 0 0 0 0 3 1 1 7 0 0 0 0 1 0 "Biology" 8 6 2 1 1 1 1 1 0 0 1 6 2 2 1 2 3 2 0 1 0 0 1 0 0 101 101 101 101 101 101 3 3 1 1 1 1 12 10 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 4 3 2 3 3 4 3 5 5 5 5 1 19 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 99 5 3 3 4 4 4 4 3 4 4 3 4 4 3 4 101 2 3 3 4 2 3 4 4 1 0 0 0 0 3 1 0 101 0 0 0 0 0 0 "Business Administration" 13 6 2 1 0 1 1 1 0 0 2 3 101 101 101 101 101 1 1 0 0 1 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 25 45 99 3 3 3 3 5 4 4 4 3 5 5 2 3 3 5 5 4 5 2 5 5 4 5 5 4 5 1 19 1 3 99 1 4 4 3 1 1 0 1)

  ,(Ans 6 5 3 3 4 4 4 3 4 4 2 3 1 2 3 4 2 1 2 1 3 1 1 2 1 1 0 0 0 0 4 0 1 99 0 1 1 0 0 0 "Undecided" 12 6 2 1 0 1 0 1 0 0 1 1 1 3 2 1 1 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 10 20 6 5 4 2 2 4 3 4 0 2 0 3 2 2 2 3 3 2 1 1 3 1 3 4 3 2 2 1 19 2 3 2 3 4 2 3 3 3 0 3)

  ,(Ans 6 3 3 1 1 3 3 3 2 4 2 3 2 3 1 3 3 1 2 2 1 1 1 1 3 1 0 0 0 0 3 0 0 7 0 0 0 0 0 0 "Business Administration" 13 6 2 1 0 0 0 0 0 0 1 1 3 2 1 1 3 1 0 0 0 0 0 0 0 3 3 3 2 2 3 2 2 2 1 1 3 10 35 6 3 2 5 5 5 4 5 3 5 3 5 2 2 3 3 3 4 2 2 3 3 3 3 3 3 99 0 19 1 4 3 3 4 2 3 3 1 0 1)

  ,(Ans 6 5 4 4 4 3 4 4 1 4 4 4 4 4 4 4 4 1 2 2 3 3 3 3 3 1 0 0 0 0 5 1 1 4 0 0 1 0 0 0 "Mechanical Engineering" 14 6 2 1 0 0 0 0 0 0 1 3 3 3 101 101 2 1 0 0 0 0 0 0 0 4 4 4 3 3 3 2 2 2 1 1 1 10 30 5 2 3 5 0 3 3 5 3 2 1 0 2 2 2 2 2 2 2 2 2 2 2 99 99 99 99 0 19 1 1 1 3 4 3 3 3 0 0 0)

  ,(Ans 6 2 3 4 4 4 4 4 4 4 4 2 2 4 4 1 1 1 1 1 2 1 1 1 1 0 0 0 1 0 4 1 1 0 0 0 0 0 0 1 "Business Administration" 16 6 2 1 1 1 0 1 0 0 1 3 2 3 1 101 3 3 1 0 1 1 0 0 0 101 101 101 101 101 101 4 4 4 4 4 4 101 101 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 19 1 1 1 3 3 3 3 3 3 3 3)

  ,(Ans 5 4 3 1 4 3 2 3 3 3 2 2 2 1 2 4 1 1 1 1 2 1 1 1 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Environmental Sciences and Resources" 12 6 99 1 0 0 0 0 0 0 1 2 3 1 1 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 50 6 4 4 4 4 4 4 5 2 3 101 5 3 3 2 101 4 4 5 3 3 3 4 4 4 3 3 1 19 2 99 99 3 3 4 3 3 3 0 3)

  ,(Ans 6 2 4 1 4 3 4 4 4 4 2 4 3 3 1 4 2 1 3 2 2 3 1 1 4 1 1 0 0 0 4 1 0 7 0 0 0 0 0 0 "Biology" 15 6 2 1 1 1 1 1 0 0 1 2 1 2 1 1 3 1 1 1 0 0 1 0 0 4 4 3 3 2 1 4 3 3 2 2 1 12 55 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 5 5 5 5 5 4 4 5 4 5 4 1 19 1 101 101 3 3 3 3 3 1 0 1)

  ,(Ans 6 4 101 1 101 4 4 4 1 3 1 4 3 4 3 4 3 1 4 1 3 3 3 3 4 0 0 0 0 1 4 101 1 7 0 0 0 0 0 1 "Psychology" 12 6 2 0 0 0 1 0 0 0 1 6 1 3 2 1 2 2 1 1 0 0 0 0 0 3 3 4 2 3 3 3 3 4 2 3 2 9 15 6 5 4 3 5 3 2 1 0 4 1 3 3 5 3 3 4 3 4 4 4 5 5 5 4 4 4 1 19 2 3 1 1 3 1 3 1 3 2 3)

  ,(Ans 6 5 3 2 3 3 4 4 2 4 3 3 3 3 3 2 3 2 3 3 3 3 2 3 1 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 5 6 2 1 0 0 0 0 0 0 1 1 2 1 1 1 3 1 1 1 0 0 0 0 0 4 3 3 2 3 3 3 2 3 2 2 2 15 45 6 4 5 3 0 4 3 4 0 3 1 3 3 4 3 4 4 4 2 3 4 3 4 4 3 3 3 1 19 1 3 5 3 4 2 3 3 3 0 3)

  ,(Ans 3 5 3 3 4 4 4 1 3 4 4 4 3 1 4 1 1 1 1 2 101 4 1 1 3 0 0 1 0 0 3 1 0 7 0 0 0 1 0 0 "Undecided" 101 6 1 1 0 1 1 0 0 0 1 3 3 1 1 2 2 3 0 0 0 0 1 0 0 4 2 2 1 4 4 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 5 4 4 2 5 5 4 2 2 3 3 3 3 1 19 1 5 5 3 4 4 3 3 0 0 0)

  ,(Ans 6 3 4 3 3 3 4 4 2 3 4 3 3 3 3 3 3 1 3 3 4 2 2 4 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Psychology" 14 5 2 1 0 1 1 1 0 0 1 6 1 2 3 1 1 2 0 1 0 0 0 0 0 4 3 3 4 4 4 3 3 3 3 2 4 20 25 6 4 5 5 3 5 4 3 3 4 4 5 2 4 3 5 5 5 3 3 3 4 4 4 4 4 4 1 19 2 1 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 3 1 4 4 3 2 2 3 2 3 3 2 2 3 1 1 1 1 1 1 1 2 1 1 0 0 0 0 4 1 1 7 1 0 0 0 0 0 "Undecided" 12 5 2 1 1 1 1 1 0 0 1 6 2 1 1 2 3 2 0 1 0 0 1 0 0 4 4 4 4 4 4 4 2 3 2 1 3 5 15 6 5 5 5 5 5 5 5 5 5 5 5 4 3 5 2 2 3 2 3 4 3 4 4 4 4 4 1 19 1 2 99 2 4 3 3 2 1 2 1)

  ,(Ans 6 2 4 4 4 4 4 4 1 4 4 4 4 4 4 1 4 4 4 4 4 4 4 4 4 1 0 0 0 0 5 1 1 1 0 0 0 0 0 0 "Middle East Studies" 18 5 2 1 0 0 0 0 0 0 1 1 3 1 1 3 2 3 1 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 101 101 1 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 99 99 99 99 1 19 2 2 5 4 4 4 4 4 3 0 3)

  ,(Ans 6 6 4 4 4 4 3 3 2 4 2 4 4 4 4 1 3 1 3 3 2 1 1 3 4 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Art" 13 5 2 1 0 0 0 0 0 0 1 6 3 1 1 1 3 2 0 0 0 0 0 0 0 101 101 101 101 101 101 3 1 2 1 2 3 16 60 4 0 0 0 0 5 5 5 4 0 101 2 1 2 1 2 3 4 1 1 2 2 1 4 4 99 99 0 19 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 3 3 2 4 4 3 3 3 2 3 1 3 3 3 4 1 3 4 4 3 2 2 4 1 0 0 0 0 4 101 1 4 0 0 0 0 0 1 "Business Administration" 13 5 2 1 1 1 1 1 0 0 1 2 1 3 1 1 1 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 4 3 3 3 4 10 30 6 3 3 3 3 2 2 1 3 2 3 3 3 3 3 3 3 2 3 3 2 3 3 3 5 4 4 1 19 2 101 2 3 3 3 3 3 1 1 1)

  ,(Ans 3 6 3 1 4 4 4 4 4 4 4 2 3 1 1 1 1 1 1 3 3 1 1 3 1 0 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "Undecided" 17 5 2 1 1 1 1 1 0 0 1 2 1 3 1 1 2 2 1 1 0 0 0 0 0 4 4 3 3 3 4 3 2 1 2 3 3 5 10 3 5 5 5 5 5 4 4 4 4 3 5 2 4 4 5 5 5 2 4 4 4 4 4 5 5 5 1 19 2 2 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 2 1 4 4 4 4 4 4 1 3 4 3 4 2 2 3 2 3 3 3 3 4 0 0 0 0 1 5 1 1 101 0 0 0 0 0 1 "Health Studies" 12 5 2 1 0 0 0 0 0 0 1 6 1 3 1 1 2 1 0 1 0 0 0 0 0 3 101 101 101 101 101 101 1 3 1 1 1 5 7 6 4 4 4 0 1 3 2 2 3 3 3 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 0 19 2 4 2 3 4 3 3 4 3 0 3)

  ,(Ans 6 5 3 1 4 4 4 2 2 4 4 4 1 1 2 1 2 1 1 1 1 3 1 1 3 1 0 0 0 0 3 1 1 1 0 0 0 1 0 0 "Undecided" 12 5 2 1 0 0 0 0 0 0 101 1 1 1 2 2 3 1 0 0 0 0 0 0 0 4 3 4 4 3 3 3 2 2 3 1 1 4 20 5 3 3 1 2 0 1 1 0 0 0 0 2 1 1 2 2 1 1 2 3 1 2 3 3 99 99 0 19 1 2 4 3 4 3 2 2 3 0 3)

  ,(Ans 6 6 4 3 4 4 4 2 3 4 4 1 4 4 4 1 3 4 4 4 4 2 2 2 4 0 0 0 0 1 4 1 1 3 0 0 0 0 0 1 "Business Administration" 13 5 99 1 0 1 0 1 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 1 4 4 4 3 3 2 3 4 4 3 3 2 101 101 6 2 3 2 5 4 2 5 1 1 2 2 2 4 2 5 5 5 5 3 3 3 4 3 4 3 3 1 19 1 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 2 3 4 4 4 3 2 4 3 4 3 3 2 2 1 1 1 1 1 1 1 2 1 1 0 0 0 0 2 1 0 99 0 0 0 0 0 1 "Business Administration" 13 5 2 1 0 0 0 0 0 0 1 2 2 1 1 1 3 2 0 0 0 0 0 0 0 3 3 1 2 2 2 2 2 2 2 2 2 10 30 3 1 1 2 2 2 2 3 2 2 2 5 2 2 2 2 2 2 2 3 2 2 2 99 99 99 99 1 19 1 4 3 3 3 1 2 2 3 2 3)

  ,(Ans 6 4 4 2 4 3 3 4 1 3 3 1 3 2 1 2 2 1 3 2 2 1 3 3 3 0 0 0 0 0 5 1 1 2 0 0 0 0 0 0 "Anthropology" 18 5 2 1 1 1 1 1 0 0 1 3 1 1 2 1 3 1 0 0 0 0 0 0 0 4 3 4 1 1 3 3 2 4 1 1 2 9 50 3 5 5 5 2 4 2 5 5 5 5 5 3 5 5 4 4 4 2 3 5 4 5 3 4 4 4 1 19 2 2 2 2 3 2 3 3 3 0 0)

  ,(Ans 6 3 3 3 1 3 4 4 2 3 3 3 3 3 3 3 3 3 3 3 3 1 3 3 3 0 1 0 0 0 3 0 0 0 0 0 0 0 0 0 "Business Administration" 17 5 2 1 0 1 1 0 0 0 1 1 1 2 2 1 2 2 1 1 0 0 1 0 0 4 4 4 4 4 4 4 2 1 2 2 4 3 10 3 0 5 5 5 5 5 5 5 5 2 5 3 4 4 5 5 4 5 4 4 3 4 3 3 3 3 1 19 1 4 2 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 4 4 4 3 3 4 3 4 3 4 3 4 3 1 2 1 3 1 2 2 1 2 2 1 0 0 0 0 4 1 1 7 0 1 1 0 0 0 "Business Administration" 12 5 2 1 0 1 1 1 0 0 3 3 1 2 1 1 3 2 0 0 0 0 1 0 0 4 4 4 2 3 2 4 4 3 2 3 2 13 25 6 5 5 5 5 5 5 5 5 5 5 5 4 5 5 3 3 4 5 4 4 4 4 4 5 5 5 1 19 2 2 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 2 4 3 4 4 4 4 2 3 4 3 1 4 2 4 4 2 4 2 101 4 2 1 2 1 0 0 0 0 3 0 1 0 1 1 1 0 0 0 "Biology" 14 5 2 1 0 1 1 0 0 0 1 1 1 2 2 1 3 1 1 1 0 1 0 0 0 4 3 2 3 4 4 4 3 2 1 3 4 14 45 6 5 5 4 5 5 4 5 101 101 2 5 2 3 3 3 5 5 3 3 3 3 5 4 5 101 5 1 19 2 2 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 4 4 3 4 4 4 4 4 4 3 4 3 4 3 3 3 3 3 3 4 101 1 3 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 14 5 2 1 0 1 1 0 0 0 3 3 1 2 2 1 3 2 1 1 1 0 0 1 0 1 101 101 1 101 101 101 1 1 101 1 1 20 40 6 4 5 2 5 4 5 5 4 5 4 5 3 4 2 5 4 3 3 4 4 3 3 5 5 4 4 1 19 1 2 2 3 3 2 3 3 2 0 1)

  ,(Ans 6 3 4 1 1 3 4 4 4 4 4 3 3 4 2 1 4 1 4 1 2 3 1 2 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Architecture" 15 4 2 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 1 1 0 0 0 0 1 4 4 4 4 4 4 2 3 3 3 3 2 101 101 6 5 5 5 5 5 5 5 5 5 2 5 2 3 4 4 4 5 3 2 4 2 2 4 4 3 3 1 19 2 3 2 3 4 2 3 3 3 2 3)

  ,(Ans 6 5 4 3 4 4 3 4 2 3 4 4 1 2 2 2 2 1 2 1 2 2 3 4 2 1 0 0 0 0 3 1 0 7 0 0 1 0 0 1 "Art" 12 4 2 1 1 1 1 1 0 0 1 6 3 1 2 1 3 3 1 0 0 0 1 1 0 3 4 2 2 2 3 3 2 2 2 3 3 101 101 6 5 5 5 3 3 2 4 2 4 2 1 3 3 2 3 4 4 5 2 3 4 3 4 4 4 4 1 19 1 2 3 3 4 2 3 3 0 0 3)

  ,(Ans 6 3 4 2 4 4 3 4 2 4 3 4 4 4 4 4 3 1 3 2 101 1 1 3 2 1 1 0 0 0 4 1 1 7 0 0 0 0 0 0 "Social Work" 15 4 2 1 0 1 1 1 0 0 1 1 101 101 101 101 3 1 0 1 0 0 0 0 0 4 4 3 2 4 3 3 3 1 1 2 2 15 20 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 4 2 2 3 3 4 4 4 4 4 4 1 19 2 3 2 3 4 3 3 101 3 0 3)

  ,(Ans 6 3 4 2 4 4 3 3 2 3 4 4 3 4 3 4 3 1 3 2 3 3 3 3 3 1 0 0 0 0 3 0 0 7 0 0 1 0 0 0 "Biology" 15 4 101 1 0 0 0 0 0 0 1 1 1 3 1 1 2 1 0 1 0 0 0 0 0 4 3 3 3 4 3 3 3 2 2 3 2 28 45 6 0 5 3 5 4 5 5 2 5 5 3 1 3 2 5 5 5 4 2 3 4 3 4 4 5 5 1 19 2 2 3 3 4 4 3 3 3 2 3)

  ,(Ans 5 5 3 1 4 4 4 4 3 4 3 2 2 2 2 1 3 1 1 1 1 2 2 2 1 1 0 0 1 0 4 1 1 7 1 0 0 0 0 0 "Liberal Studies" 13 4 2 1 0 1 1 0 0 0 1 1 1 1 1 1 3 2 0 0 0 0 0 0 0 4 4 3 2 4 3 3 3 3 2 3 2 20 35 5 0 3 3 0 3 3 3 1 0 0 101 1 2 2 2 2 2 1 1 2 3 3 3 4 99 99 0 19 2 2 2 3 4 3 3 3 1 1 0)

  ,(Ans 3 5 2 1 1 4 4 1 1 1 4 4 4 1 4 1 2 1 1 1 1 1 3 3 1 0 0 0 0 1 4 1 1 1 0 0 0 0 0 1 "Anthropology" 12 4 99 0 0 1 0 0 0 0 101 6 3 1 2 3 3 2 0 1 0 0 0 0 0 3 3 3 2 3 3 3 2 3 1 1 1 4 20 1 3 4 4 5 5 2 2 3 3 5 3 3 4 4 4 4 3 3 4 5 4 3 4 4 3 3 1 19 1 4 4 3 2 4 2 3 0 0 0)

  ,(Ans 6 6 4 4 4 4 4 4 3 4 3 4 4 4 4 4 3 3 3 3 3 3 3 3 3 1 0 0 0 0 4 1 1 7 0 1 0 0 0 0 "Electrical Engineering" 12 4 99 1 0 0 0 0 0 1 1 2 3 1 1 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 40 120 6 5 5 4 3 2 3 3 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 4 3 4 3 1 19 1 1 2 3 4 4 3 3 0 0 0)

  ,(Ans 5 3 2 3 4 3 4 3 3 4 3 3 2 3 3 2 3 1 3 2 3 2 3 3 3 1 0 0 0 0 4 0 1 2 1 0 1 0 0 0 "Child and Family Studies" 13 4 2 1 0 1 0 0 0 0 1 3 1 1 3 1 1 1 0 1 0 0 0 0 0 3 3 2 2 3 3 3 3 2 1 3 3 10 35 4 4 4 4 5 2 1 1 1 2 1 5 3 3 2 2 2 1 1 2 3 2 4 4 4 4 4 1 19 2 2 1 2 3 3 3 3 1 0 1)

  ,(Ans 6 6 3 1 2 4 4 3 1 3 3 3 3 3 2 1 3 1 3 3 4 1 1 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Art" 14 4 2 1 0 0 0 0 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 0 3 4 4 3 3 4 3 4 4 3 3 4 101 101 5 4 4 2 5 5 5 5 5 5 4 5 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 1 19 2 2 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 3 3 4 3 3 1 3 4 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 0 0 0 4 1 1 7 0 0 0 0 0 0 "International Studies" 12 4 2 1 0 0 1 0 0 0 1 1 2 1 1 3 1 2 0 0 0 0 0 0 0 2 2 2 2 2 2 101 101 101 101 101 101 1 8 1 2 5 5 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 1 19 1 3 3 3 3 101 3 3 3 3 3)

  ,(Ans 6 3 4 4 4 2 2 4 1 4 2 4 2 3 4 1 1 1 1 1 1 1 1 1 3 1 0 0 0 0 3 1 0 7 0 0 0 0 0 1 "Political Science" 13 4 2 1 0 1 0 0 0 0 1 2 3 1 1 1 2 3 1 0 0 0 1 0 1 3 3 3 3 3 3 101 3 3 3 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 2 5 4 4 5 5 4 4 3 3 1 19 1 5 4 1 1 1 3 3 3 3 3)

  ,(Ans 6 3 4 3 3 3 4 2 2 2 3 4 4 3 3 4 3 1 1 1 1 3 2 1 1 1 0 0 0 1 4 0 1 0 0 0 0 0 0 0 "Business Administration" 17 4 2 1 0 1 0 0 0 0 1 1 2 1 1 2 3 1 0 0 0 0 0 0 0 3 2 2 3 3 4 3 2 2 1 1 3 12 60 5 2 2 4 3 5 2 2 3 3 0 3 3 4 4 3 3 4 2 3 3 1 3 4 3 2 2 1 19 1 3 3 3 4 3 3 2 0 0 3)

  ,(Ans 6 6 4 3 4 4 4 3 2 4 4 4 4 4 4 4 3 2 3 4 3 3 4 3 3 1 0 0 0 0 2 1 1 7 0 0 0 1 0 0 "Undecided" 13 4 2 1 0 1 0 0 0 0 1 2 1 1 1 1 3 1 1 0 0 0 0 0 0 4 3 3 2 4 3 101 101 101 101 101 101 20 40 6 5 5 5 5 5 0 5 0 5 5 3 3 3 3 3 3 1 3 2 3 2 3 4 4 3 3 0 19 1 2 2 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 3 4 4 4 4 4 4 1 4 4 4 3 3 3 3 3 3 2 1 3 1 0 0 0 0 4 0 1 4 0 0 0 0 0 0 "Accounting" 17 4 101 1 0 1 1 1 0 0 1 6 3 2 1 1 3 1 0 0 0 0 1 0 0 4 4 4 4 4 4 2 1 4 3 4 2 20 60 6 2 5 2 0 5 0 2 0 2 2 5 1 4 3 5 5 1 2 1 4 3 4 3 4 3 3 1 19 1 3 101 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 4 4 4 4 4 4 2 3 4 2 2 4 3 2 2 4 4 4 4 3 4 4 4 1 1 0 0 0 4 1 1 0 0 0 0 0 1 0 "Business Administration" 17 4 1 1 0 1 1 1 0 0 2 6 3 2 1 2 2 2 1 1 0 0 0 1 1 2 2 2 2 2 2 101 101 101 101 101 101 1 3 6 3 5 5 5 3 3 3 3 3 3 2 3 3 4 4 4 5 3 3 3 3 3 4 4 4 4 1 19 1 4 5 3 3 4 3 2 0 0 0)

  ,(Ans 3 5 3 1 2 3 3 1 3 3 4 3 4 1 1 1 3 1 1 1 1 3 3 3 1 1 0 0 0 0 2 1 1 4 0 0 0 0 0 1 "Undecided" 12 4 3 1 0 1 1 0 0 0 1 6 3 1 1 2 1 3 0 0 0 0 1 0 0 4 1 3 3 3 3 4 1 2 2 3 4 101 101 6 3 5 4 0 0 5 3 2 2 1 3 1 1 1 1 1 1 1 1 1 2 1 1 3 1 1 0 19 1 4 5 3 4 4 3 3 1 0 0)

  ,(Ans 6 3 3 2 2 3 3 4 1 2 1 4 3 3 3 101 3 1 3 1 1 1 1 1 1 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Computer Science" 16 4 3 1 0 1 1 1 0 0 1 1 2 1 1 1 2 2 0 0 0 0 0 0 0 3 3 3 3 3 3 2 2 2 2 2 2 5 15 5 5 5 5 0 3 5 5 4 5 3 5 4 5 5 4 3 4 3 4 4 3 5 3 3 3 3 0 19 1 4 2 3 4 4 3 3 1 0 1)

  ,(Ans 5 5 3 1 3 4 3 3 2 3 4 4 3 3 3 4 3 2 3 3 4 1 1 2 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Biology" 20 4 2 1 0 1 1 0 0 0 1 1 1 1 1 2 3 1 0 0 0 0 1 1 0 4 3 3 2 2 3 4 3 2 3 2 3 30 40 6 2 4 2 4 2 2 3 3 2 1 2 2 3 3 2 4 1 3 2 3 4 5 5 4 3 3 99 19 1 5 5 3 4 3 3 3 3 3 3)

  ,(Ans 6 1 4 3 4 4 4 4 2 3 4 3 3 3 4 1 2 1 3 3 4 1 4 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Computer Science" 13 4 1 1 1 1 0 0 0 0 1 6 3 1 2 1 3 3 0 0 0 0 0 0 0 1 2 2 1 2 2 101 101 101 101 101 101 101 101 5 4 5 5 5 5 5 5 5 5 2 3 3 4 3 4 4 5 4 5 4 3 5 3 3 4 4 1 19 1 2 3 3 3 3 3 3 1 1 1)

  ,(Ans 3 3 4 3 4 4 4 4 3 4 4 3 2 2 3 1 3 1 3 4 3 3 3 4 3 1 0 0 0 0 3 1 1 4 0 0 0 0 0 1 "Electrical Engineering" 17 4 1 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 1 0 1 0 0 1 0 4 3 2 2 4 3 3 3 3 2 1 3 101 101 6 5 2 4 1 0 2 5 0 0 0 5 3 3 3 4 3 2 2 4 3 2 4 4 3 5 5 0 19 1 2 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 101 4 4 3 4 4 3 2 4 3 3 3 4 3 2 3 1 4 3 3 2 2 2 3 1 0 0 0 0 4 0 1 7 0 0 0 0 0 0 "Business Administration" 12 4 2 1 0 1 1 1 0 0 1 2 2 101 101 101 3 101 0 0 0 0 0 0 0 4 3 4 3 3 2 101 101 3 101 101 1 10 40 6 1 5 5 5 5 5 5 5 5 5 5 4 4 4 4 3 3 3 4 3 4 3 4 4 4 4 1 19 1 2 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 2 4 4 4 4 4 3 4 3 3 3 4 3 3 3 2 4 3 2 1 2 2 2 1 0 0 0 0 3 0 0 2 0 0 1 0 0 0 "Administration of Justice" 15 4 2 1 0 1 0 1 0 0 1 3 1 3 2 1 2 1 0 0 0 1 0 0 0 4 4 3 3 3 4 4 3 3 2 2 4 30 45 6 5 5 4 4 5 5 5 4 5 4 5 3 2 2 5 5 3 4 4 3 3 4 4 4 5 5 1 19 2 2 4 3 3 2 3 3 0 2 3)

  ,(Ans 6 3 4 1 4 3 3 4 2 4 2 2 4 4 1 3 1 1 1 1 1 1 2 1 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 17 4 2 1 0 0 0 0 0 0 1 2 101 2 101 101 3 3 1 0 0 0 0 0 0 3 2 2 3 4 1 1 1 1 1 4 1 101 101 6 5 4 5 5 5 5 5 5 5 5 1 4 5 3 2 2 5 5 3 3 3 4 3 5 2 2 1 19 1 3 1 3 4 1 2 3 3 2 3)

  ,(Ans 6 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 2 4 2 3 2 3 3 4 0 0 1 0 0 4 1 1 7 1 0 0 0 0 0 "Health Studies" 16 4 101 1 0 1 0 0 0 0 1 2 2 101 3 101 2 2 1 1 0 0 1 0 0 3 4 3 2 3 4 3 4 2 1 2 2 2 15 6 5 5 5 4 5 2 4 5 5 2 5 3 4 2 4 4 3 1 3 3 3 3 4 4 3 3 0 19 2 4 4 3 3 101 3 3 3 0 3)

  ,(Ans 6 4 4 1 4 4 4 4 2 4 2 3 2 4 2 2 2 1 3 4 3 1 2 1 3 1 0 0 0 0 4 1 0 0 1 0 0 0 0 0 "Psychology" 12 4 2 1 0 1 0 1 0 0 1 2 1 1 2 1 3 2 0 0 0 0 0 0 0 3 3 3 3 3 3 3 2 2 2 1 1 15 45 5 5 5 5 2 5 2 5 3 3 5 1 2 3 3 3 3 3 3 3 3 2 2 4 4 2 5 1 19 2 3 1 3 3 3 3 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 4 3 3 4 4 4 4 3 3 3 4 1 2 2 2 1 2 3 4 1 0 0 0 0 4 1 1 7 1 0 1 0 0 0 "Undecided" 12 4 2 1 0 1 1 1 0 0 1 2 1 2 2 1 3 1 1 0 0 0 0 0 0 4 3 4 3 4 4 3 3 4 3 3 4 101 55 6 4 3 5 5 5 3 5 4 5 5 5 2 3 3 5 4 4 3 3 4 4 4 3 4 3 3 0 19 2 2 2 3 3 2 2 3 3 0 3)

  ,(Ans 6 3 4 2 2 4 4 4 1 4 4 4 3 3 3 4 3 1 2 2 2 1 1 2 4 0 1 0 0 0 3 1 1 7 0 0 1 0 0 0 "Mechanical Engineering" 12 4 2 0 0 1 1 1 0 0 1 6 3 101 101 101 3 2 0 0 0 0 0 0 0 4 3 3 2 3 4 3 3 3 3 3 3 15 45 5 5 5 5 5 5 5 3 5 3 3 5 3 3 3 101 3 4 3 5 3 3 3 3 3 99 99 0 19 1 4 2 3 3 3 101 3 3 2 3)

  ,(Ans 6 3 3 1 4 4 4 4 4 4 3 4 1 3 3 3 3 1 3 3 3 3 1 3 3 1 0 0 0 0 3 0 1 7 0 1 1 0 0 0 "Chemistry" 14 4 3 1 0 1 1 0 0 0 1 3 1 2 2 1 3 1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 20 90 6 101 101 101 101 101 101 101 101 101 101 101 1 1 1 1 1 1 1 1 1 1 1 5 5 5 5 1 19 2 2 3 3 3 2 3 3 1 2 1)

  ,(Ans 6 5 3 1 2 4 4 1 3 3 4 3 4 3 3 1 3 1 3 2 3 1 1 4 3 1 0 0 0 0 3 1 1 99 0 0 0 1 0 0 "International Studies" 12 4 101 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 1 0 0 1 0 1 3 4 3 3 3 3 3 3 2 2 3 2 101 101 6 5 5 3 5 5 3 5 0 5 3 5 5 5 3 5 5 3 5 3 4 5 4 4 4 4 4 1 19 1 5 5 3 4 4 3 3 1 1 1)

  ,(Ans 6 5 4 3 4 4 4 4 3 4 3 3 4 3 3 1 4 2 3 3 3 1 1 4 2 1 0 0 0 0 4 1 1 7 1 1 1 0 0 0 "Undecided" 12 3 101 1 0 1 0 0 0 0 1 2 3 101 101 101 101 3 0 0 0 0 0 0 0 3 2 2 2 2 3 101 101 101 101 101 101 101 101 6 5 5 5 0 5 5 5 0 5 0 5 3 5 5 2 3 5 2 1 4 2 5 2 2 1 1 0 19 1 4 4 3 4 4 2 3 3 0 3)

  ,(Ans 6 3 3 1 4 4 4 3 3 4 3 2 4 3 4 1 4 3 4 3 4 2 4 2 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Psychology" 13 3 101 1 0 1 0 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 1 1 0 4 3 2 3 4 4 3 2 2 2 4 4 101 101 6 5 5 5 5 5 2 5 2 4 2 2 4 5 4 4 5 3 3 2 4 3 3 4 3 4 3 1 19 2 2 2 3 3 4 3 3 1 1 1)

  ,(Ans 6 5 4 1 4 4 4 2 1 4 4 3 4 4 4 1 3 1 3 1 1 1 2 1 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Psychology" 12 3 2 1 0 1 1 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 0 0 0 4 4 4 4 4 4 4 4 3 3 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 4 4 4 4 4 4 4 4 1 1 1 1 0 19 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 6 4 3 3 4 3 2 1 4 4 1 3 3 3 1 3 1 3 1 3 3 4 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "English" 14 3 99 0 0 0 0 0 0 1 1 6 3 1 2 1 2 3 0 0 0 0 0 0 0 4 4 3 3 4 4 3 2 2 2 3 2 101 101 6 5 5 5 5 3 2 5 3 5 5 2 4 5 4 4 4 3 5 3 5 5 4 5 5 4 5 1 19 1 99 5 3 4 4 3 3 0 0 1)

  ,(Ans 6 6 4 2 2 4 4 2 2 2 3 3 4 4 4 3 3 3 4 4 4 1 2 4 4 1 0 0 0 0 5 1 1 7 0 0 1 0 0 0 "Psychology" 14 3 2 1 0 1 1 0 0 0 1 1 101 3 101 101 101 1 1 1 0 0 0 0 0 101 101 101 101 101 101 4 3 3 2 3 3 13 40 6 5 5 5 4 5 5 3 3 4 2 5 3 3 3 4 4 3 2 2 2 3 3 4 4 4 4 1 19 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 3 4 2 2 4 1 1 4 4 4 1 1 1 1 3 3 1 1 4 2 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Accounting" 17 3 2 1 0 1 1 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 3 3 4 3 2 2 2 3 3 101 101 6 5 5 5 0 5 3 5 0 3 1 3 3 2 3 2 1 3 3 1 3 2 4 2 4 2 2 0 19 2 2 1 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 3 4 4 4 3 3 4 3 4 3 2 1 1 1 1 1 2 2 1 3 3 3 1 0 0 0 0 4 1 1 7 1 1 0 0 1 0 "Environmental Studies" 14 3 2 1 0 0 0 0 0 0 1 2 3 1 1 1 2 3 0 0 1 0 0 0 0 4 3 2 2 2 3 3 3 2 2 2 3 101 101 6 5 5 5 4 5 5 5 4 3 2 1 3 4 2 3 3 4 2 3 3 3 3 4 4 99 3 0 19 1 3 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 2 4 4 4 2 4 4 2 2 2 3 3 2 2 1 3 2 2 2 3 1 1 0 1 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 15 3 101 1 0 0 0 0 0 0 3 3 1 2 2 1 1 2 0 0 0 0 1 0 0 3 4 1 1 4 101 101 101 4 1 101 1 4 10 4 4 1 2 4 5 3 2 2 3 5 3 2 2 3 3 4 5 1 1 2 2 4 5 3 3 3 0 19 1 2 4 2 101 4 3 2 0 0 0)

  ,(Ans 3 5 4 3 4 4 4 1 2 4 4 3 4 2 2 1 4 1 4 4 3 1 1 3 2 1 1 0 0 0 4 0 1 0 0 0 0 0 0 0 "Undecided" 13 3 1 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 4 4 4 4 2 2 2 3 3 101 101 6 0 5 4 0 3 0 5 3 4 1 5 1 101 3 4 4 1 1 3 3 2 2 3 3 3 3 0 19 1 3 2 3 4 4 3 3 0 0 3)

  ,(Ans 6 1 1 2 3 4 4 4 4 4 3 4 3 4 3 3 3 3 3 1 3 2 2 2 2 1 0 0 0 0 3 1 0 7 0 1 1 0 0 0 "Art" 17 3 2 1 0 1 0 0 0 0 1 3 1 1 2 1 3 1 0 0 0 0 0 0 0 3 4 3 3 3 2 101 101 101 101 101 101 7 45 6 2 2 1 1 1 1 1 0 2 0 5 3 2 3 3 2 3 3 2 2 2 3 3 4 3 4 1 19 2 1 1 3 3 1 2 2 0 0 0)

  ,(Ans 5 6 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "English" 15 3 3 1 0 0 0 0 0 0 1 6 3 101 101 101 101 3 0 0 0 0 0 0 0 3 1 1 1 3 1 3 1 1 1 4 1 101 101 3 5 5 3 3 3 5 0 5 5 1 5 5 5 5 5 5 5 4 5 5 3 5 3 5 3 3 0 19 1 5 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 4 3 4 4 4 4 3 4 1 3 3 4 3 3 2 3 3 1 2 3 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Biology" 15 3 2 1 1 1 1 1 0 0 1 1 1 3 2 1 1 1 0 1 0 0 0 0 0 4 4 3 3 2 4 3 4 3 3 2 4 15 25 6 0 5 4 5 5 5 5 4 5 3 5 2 5 5 5 5 101 5 3 3 3 5 3 5 3 3 1 19 2 2 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 3 4 1 4 4 3 3 2 4 4 4 4 2 1 1 2 1 3 1 3 2 1 4 1 1 0 0 0 0 3 1 1 0 0 0 1 1 0 1 "Social Work" 15 3 101 1 0 1 0 1 0 0 1 2 3 1 1 2 2 3 0 0 0 0 1 1 0 4 3 4 3 4 2 4 3 2 1 4 1 101 101 6 3 5 5 5 5 2 5 1 3 2 1 3 2 4 4 101 2 4 1 4 5 2 4 2 2 4 1 19 2 5 5 3 3 4 3 4 3 3 3)

  ,(Ans 6 5 4 2 4 4 3 2 4 4 4 4 4 3 2 1 3 1 3 2 4 3 1 3 4 0 1 0 0 0 3 1 0 7 0 1 1 0 0 0 "Undecided" 13 3 2 1 0 0 0 1 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 1 3 2 2 2 3 3 3 2 2 1 2 3 101 101 6 5 5 4 2 5 4 5 3 4 4 5 3 2 2 5 5 3 2 2 4 4 4 4 5 2 2 1 19 2 5 5 3 4 3 3 3 3 2 3)

  ,(Ans 3 5 4 4 4 4 4 4 3 4 1 2 2 4 2 3 3 2 3 3 3 2 2 1 2 1 0 0 0 0 3 1 1 101 0 0 0 0 0 0 "Architecture" 15 3 3 1 1 1 1 1 0 0 2 6 1 2 2 101 3 1 1 0 0 0 0 0 0 2 2 3 3 4 3 2 2 3 3 3 3 10 35 6 5 5 5 5 5 5 5 5 0 0 5 1 2 3 4 4 3 4 3 3 3 4 4 4 3 4 1 19 1 4 3 3 3 1 3 3 0 0 0)

  ,(Ans 6 6 3 1 4 3 4 1 2 4 3 3 3 3 3 1 2 1 2 2 1 1 1 2 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Environmental Sciences and Resources" 13 3 99 1 0 1 1 1 0 1 1 6 3 1 1 3 2 3 1 0 0 0 1 1 0 4 4 4 3 3 4 3 3 2 2 2 3 101 101 5 2 4 4 5 5 3 2 2 3 0 5 2 3 2 4 4 4 2 2 3 2 3 4 4 99 99 1 19 1 4 4 3 3 3 3 3 1 1 1)

  ,(Ans 5 1 3 3 4 4 4 4 4 4 4 1 1 4 4 1 4 4 4 4 4 4 2 3 1 0 0 0 0 1 4 1 1 0 1 1 0 0 0 0 "Psychology" 8 3 2 1 1 1 0 1 0 0 1 1 1 3 1 1 1 1 0 0 0 0 1 0 0 3 3 2 3 3 3 3 2 2 2 2 2 15 25 6 5 5 5 5 5 5 5 5 5 5 5 2 3 3 3 3 3 3 3 3 4 5 3 3 2 3 1 19 1 5 5 3 3 3 3 3 0 0 1)

  ,(Ans 6 2 3 1 4 4 4 4 2 4 2 4 2 3 3 3 3 3 3 2 3 1 1 1 4 1 1 0 1 0 3 0 0 0 0 1 0 0 0 0 "Science" 12 3 2 1 0 1 0 1 0 0 2 3 1 3 2 1 1 2 0 1 0 0 0 0 0 4 101 101 2 101 101 101 4 3 101 1 101 20 40 5 2 3 2 0 0 3 3 0 0 2 5 2 2 2 2 2 2 2 2 2 2 2 4 4 3 3 0 19 1 2 1 2 3 2 2 2 1 2 1)

  ,(Ans 6 3 4 4 4 4 4 4 3 4 4 3 4 4 3 3 4 3 4 4 4 3 4 4 4 1 0 0 0 0 5 1 1 1 0 0 0 0 0 1 "Elementary Education" 12 3 2 1 0 1 0 1 0 0 1 3 2 2 2 2 3 2 1 0 0 0 1 1 0 4 3 4 3 3 3 3 3 3 3 3 3 6 30 5 3 3 3 4 3 3 3 3 3 3 2 2 2 2 3 3 3 5 3 3 4 4 4 5 4 4 1 19 1 3 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 4 4 4 4 4 2 4 4 4 4 4 1 1 3 1 2 2 3 1 1 4 2 1 0 0 0 0 4 0 1 0 0 1 0 0 0 0 "Film" 15 3 2 1 0 0 0 0 0 0 1 1 3 1 2 2 3 3 1 1 0 0 0 0 0 4 3 3 4 4 4 3 1 3 3 4 4 101 101 1 5 5 5 5 5 5 5 5 5 2 5 4 3 3 5 5 4 5 4 3 3 5 5 5 5 5 1 19 1 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 1 1 3 4 4 1 2 3 2 4 2 4 1 4 2 1 2 2 1 2 1 1 1 0 1 0 1 0 4 0 1 2 0 0 1 0 1 0 "Administration of Justice" 16 3 2 1 0 1 1 0 0 0 1 1 1 2 2 1 3 1 0 0 0 0 0 0 0 3 2 2 1 3 3 2 1 1 1 3 2 101 60 1 0 5 5 5 5 2 2 2 5 1 5 3 5 4 3 3 3 1 3 4 2 4 3 4 2 2 1 19 2 4 4 3 4 2 3 3 1 0 1)

  ,(Ans 6 5 3 1 2 4 4 2 2 3 3 3 3 3 3 3 3 1 3 3 2 1 2 2 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Environmental Sciences and Resources" 14 3 2 1 0 1 0 0 0 0 1 2 101 101 101 2 3 2 0 1 0 0 0 1 0 4 4 3 2 2 4 4 3 2 2 2 4 3 20 5 4 5 5 4 5 4 4 5 5 4 4 3 3 3 3 4 3 3 3 3 3 3 4 5 4 4 1 19 1 5 4 3 4 3 3 3 1 1 1)

  ,(Ans 3 5 4 2 3 4 4 3 3 3 3 4 4 3 1 1 2 1 2 2 2 1 1 2 1 1 0 0 0 0 4 1 0 7 0 0 1 1 0 0 "Undecided" 15 3 2 1 0 1 1 0 0 0 1 2 3 2 101 101 2 3 1 0 0 0 0 0 0 3 2 3 2 3 4 3 2 2 1 3 4 101 101 5 5 5 5 5 5 5 5 5 5 5 5 3 5 4 4 3 3 4 5 5 2 5 3 4 4 4 1 19 2 3 3 3 4 2 2 2 3 3 3)

  ,(Ans 6 6 3 1 4 4 4 3 2 4 3 2 2 4 2 1 3 1 3 1 1 1 1 4 1 1 1 0 1 0 5 1 0 4 0 0 0 0 1 0 "Foreign Languages" 17 3 2 1 0 1 1 1 0 0 1 1 3 2 2 2 3 2 1 0 0 0 0 0 0 4 4 4 4 4 4 4 3 4 2 4 2 16 36 5 5 5 5 3 3 2 5 0 4 3 5 4 4 3 5 5 4 5 3 5 5 5 3 5 3 4 1 19 1 4 3 3 4 2 3 3 3 3 3)

  ,(Ans 6 3 4 2 4 2 4 4 4 4 3 4 1 4 3 3 3 1 4 3 2 1 1 1 1 1 0 0 0 0 2 1 1 4 0 0 0 0 0 1 "Accounting" 14 3 3 1 1 1 1 1 0 0 1 1 1 1 2 1 3 1 0 0 0 0 0 0 0 4 4 3 3 4 3 3 3 2 2 4 3 15 80 5 4 4 1 0 4 1 3 1 1 1 1 2 3 2 4 4 3 3 2 2 2 4 3 4 2 2 1 19 2 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 2 4 3 4 4 4 4 4 4 3 4 4 3 3 4 4 4 4 4 4 3 3 4 4 1 0 0 0 0 4 1 101 7 0 1 0 0 0 0 "Science" 13 3 99 1 0 1 1 0 0 0 101 3 1 3 1 1 3 1 1 1 0 0 0 0 0 3 3 3 2 3 3 3 3 3 2 3 3 101 60 2 3 4 4 4 4 4 3 3 3 3 3 3 3 3 3 4 3 3 3 4 4 4 3 4 3 4 0 19 2 2 2 3 2 2 3 3 0 2 0)

  ,(Ans 6 6 3 4 4 4 4 2 3 4 2 4 2 3 2 3 2 1 2 2 2 2 2 2 3 0 1 0 0 0 3 0 1 7 0 0 0 0 0 1 "Undecided" 15 3 2 1 1 1 1 0 0 0 1 2 1 3 2 1 2 1 1 1 0 0 0 0 0 3 3 3 3 3 3 3 2 2 2 3 3 10 20 99 5 5 5 5 5 4 4 3 3 2 2 3 3 3 3 3 2 2 3 3 2 3 4 4 4 4 1 19 2 4 4 3 3 2 3 3 1 0 1)

  ,(Ans 6 3 4 4 4 3 3 4 1 4 4 4 4 3 1 1 3 1 3 1 4 1 1 4 3 1 1 0 0 0 3 1 0 7 0 0 1 0 0 0 "Did not answer" 14 3 2 1 0 1 1 1 0 0 1 6 3 1 1 2 3 3 1 1 0 0 0 0 0 4 3 3 2 3 4 3 2 3 1 3 4 101 101 99 5 5 3 5 5 4 5 2 5 5 4 4 4 4 5 5 3 3 3 3 4 2 4 5 3 5 0 19 2 2 2 1 3 1 3 3 1 1 1)

  ,(Ans 6 3 4 4 4 4 4 4 2 1 2 4 3 3 4 1 2 1 3 1 3 3 1 3 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Theater Arts" 14 3 2 1 0 1 1 1 0 0 1 6 3 1 1 2 3 3 1 0 0 0 0 0 0 4 3 3 3 2 4 4 3 4 3 1 4 101 101 6 5 5 4 5 5 5 3 5 5 5 5 3 4 3 4 3 5 2 3 4 4 4 4 5 5 5 1 19 99 2 4 3 3 4 3 2 1 1 1)

  ,(Ans 6 3 4 1 1 4 4 4 4 4 4 3 3 3 3 1 3 1 3 2 1 101 2 4 1 1 1 0 0 0 4 1 1 0 0 0 0 0 0 0 "Art" 26 3 2 1 0 1 0 0 0 0 1 1 2 1 1 1 3 2 1 0 0 0 1 0 0 3 2 101 3 4 4 3 2 101 1 4 2 8 20 3 5 5 5 5 5 5 5 5 5 5 5 3 4 4 4 3 4 4 2 4 4 4 4 4 4 4 1 19 2 4 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 3 3 4 3 3 1 3 4 3 4 4 3 2 3 3 1 4 3 2 2 3 3 3 0 0 0 0 1 3 0 0 0 0 0 1 0 0 0 "International Studies" 13 3 2 1 0 1 0 1 0 0 1 2 3 101 101 101 3 1 0 0 0 0 0 0 0 3 4 4 3 3 3 2 2 2 1 2 1 13 75 3 3 2 2 0 1 1 1 0 2 0 1 1 1 1 2 2 2 2 1 2 3 2 3 3 3 3 0 19 2 2 3 3 3 4 3 2 1 2 1)

  ,(Ans 3 101 3 1 1 3 4 2 3 3 4 2 2 2 2 3 3 1 3 3 3 3 1 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 14 3 2 1 0 1 1 1 0 0 1 2 101 101 101 101 3 1 1 0 0 0 0 1 0 3 3 3 3 3 3 3 3 3 3 3 3 20 20 4 2 5 5 5 5 5 5 5 5 5 5 2 3 4 5 5 5 3 3 4 4 4 4 4 4 4 1 19 1 4 5 3 4 3 3 3 3 0 3)

  ,(Ans 5 2 2 1 4 3 4 4 4 4 4 1 4 3 4 1 3 1 3 3 3 3 1 2 1 1 0 0 0 0 4 1 0 4 0 1 1 0 0 0 "" 14 3 2 1 0 1 0 0 0 0 1 3 3 1 1 2 1 3 0 1 0 0 1 0 0 4 3 2 1 3 4 3 3 3 1 3 4 101 101 5 3 5 5 5 5 4 2 5 5 5 5 2 3 3 5 5 5 2 3 4 4 5 3 3 4 4 1 19 2 4 4 3 4 4 2 3 3 0 3)

  ,(Ans 3 4 4 3 4 4 4 4 4 3 4 4 4 4 4 4 4 1 4 3 3 4 3 3 4 1 0 0 0 0 3 1 1 4 1 1 1 0 0 0 "History" 15 3 2 1 0 1 1 0 0 0 1 6 3 1 1 1 3 1 1 0 0 0 0 0 0 4 4 3 3 4 3 3 3 2 2 2 2 30 45 6 5 5 5 0 2 3 1 2 2 0 5 3 3 3 3 3 3 2 1 3 3 3 3 3 3 3 0 19 2 3 3 3 4 4 3 3 0 0 0)

  ,(Ans 3 3 3 4 4 4 4 4 2 2 4 4 4 2 2 1 2 1 2 1 3 3 2 3 4 0 1 0 0 0 4 1 1 0 0 0 0 1 0 0 "Did not answer" 16 3 2 1 1 1 0 1 0 0 1 6 1 2 2 1 3 2 1 1 0 0 1 0 0 3 3 3 2 3 2 3 2 1 1 1 1 5 15 3 5 5 5 5 5 3 4 3 3 3 2 5 5 5 5 5 3 3 3 4 3 4 4 4 4 4 1 19 1 4 5 3 4 3 3 3 1 1 1)

  ,(Ans 5 3 3 3 3 4 4 3 1 2 3 4 4 3 1 1 2 1 3 1 2 2 1 2 4 1 0 0 0 0 4 0 0 0 0 0 0 0 0 1 "International Studies" 16 3 99 0 0 1 1 0 0 1 1 6 3 1 1 2 3 3 1 0 1 0 0 1 1 3 3 3 2 3 3 3 3 3 2 3 3 101 101 5 2 5 5 2 1 3 2 5 5 2 2 4 5 5 3 3 4 4 5 5 4 4 4 4 3 3 0 19 1 5 4 3 4 2 2 2 3 3 3)

  ,(Ans 6 3 4 1 4 4 3 3 2 3 4 3 3 4 3 1 2 1 2 2 3 2 3 4 2 0 0 0 0 1 5 1 1 7 0 0 0 0 0 0 "Film" 17 3 2 1 1 1 1 1 0 0 1 1 1 1 1 2 3 2 0 1 0 0 0 0 0 4 3 4 2 4 3 4 2 3 2 4 2 5 40 6 5 5 5 5 5 5 5 5 5 5 5 4 3 2 5 4 4 3 3 4 5 3 5 5 4 4 1 19 2 2 2 3 4 2 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 4 4 4 3 4 2 4 4 1 3 4 3 1 1 3 1 1 3 1 1 0 1 0 3 101 0 0 0 0 0 0 0 1 "Health Studies" 18 3 1 1 1 1 0 1 0 0 101 6 1 2 101 101 2 2 1 0 0 0 0 0 0 4 4 4 3 3 3 4 4 3 3 3 3 12 15 6 5 5 5 1 5 4 5 0 5 5 5 4 4 4 3 4 3 4 2 4 3 4 4 5 4 5 1 19 1 2 2 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 3 1 3 4 4 4 3 3 4 3 4 2 2 4 3 1 3 3 3 3 4 4 1 0 1 0 0 0 3 1 0 4 0 0 1 0 0 0 "Art" 18 3 2 1 0 1 1 1 0 0 1 1 1 2 2 2 3 1 0 1 0 0 0 0 0 4 2 3 2 1 2 3 2 2 1 1 2 5 40 6 3 5 4 2 5 4 5 2 5 3 2 3 4 3 4 5 3 4 2 4 3 4 5 4 3 3 1 19 2 5 5 3 3 2 3 3 1 1 1)

  ,(Ans 6 3 3 1 4 3 3 4 2 3 2 3 3 4 3 1 2 1 3 2 2 3 3 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Music" 15 3 2 1 0 1 0 0 0 0 1 1 3 101 101 2 2 3 0 0 0 0 0 0 1 4 3 3 2 3 3 4 3 3 2 3 2 101 101 4 1 2 5 5 2 0 0 0 5 4 0 2 3 3 5 3 3 4 3 4 4 4 3 3 99 99 0 19 2 99 3 3 4 3 3 3 1 1 1)

  ,(Ans 6 6 4 3 3 4 4 3 2 4 2 4 4 4 3 4 4 3 4 3 4 4 1 3 3 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Computer Science" 14 3 2 1 0 1 0 0 0 0 1 2 1 3 2 1 2 1 1 1 0 0 0 0 0 4 3 4 3 3 3 4 3 4 3 3 3 10 30 6 5 5 4 101 1 2 3 1 4 1 4 3 4 3 2 2 4 1 3 3 2 4 4 4 4 4 1 19 1 4 4 3 4 3 2 3 1 2 1)

  ,(Ans 3 3 3 3 4 4 4 3 2 4 4 4 2 3 4 3 4 3 3 4 4 3 4 3 3 1 0 1 0 0 2 0 1 0 1 1 0 0 0 1 "Accounting" 16 3 3 1 0 1 0 0 0 0 1 1 2 1 1 1 3 3 1 0 0 0 0 1 0 4 4 4 4 3 3 4 4 4 4 4 4 101 101 6 0 0 101 2 3 0 0 1 2 1 0 1 2 2 1 1 1 2 3 1 1 1 5 4 5 5 1 19 2 2 4 3 4 4 3 3 2 2 2)

  ,(Ans 6 3 4 3 3 4 4 4 3 4 4 3 4 3 3 2 3 2 4 3 4 2 2 4 3 1 0 0 0 0 4 0 0 0 0 0 0 1 0 0 "Accounting" 14 3 1 1 0 0 1 0 0 0 1 1 1 1 1 1 3 2 1 1 0 0 0 1 0 4 4 3 4 4 3 4 3 3 4 4 3 15 45 6 3 3 2 1 0 0 3 3 0 0 1 1 1 1 1 1 2 2 2 1 2 1 4 5 5 5 1 19 2 4 4 3 4 4 3 3 0 0 0)

  ,(Ans 6 5 4 3 3 4 4 3 3 4 4 2 3 3 3 3 3 2 3 3 3 2 2 2 2 1 0 0 0 0 3 1 1 2 0 0 1 0 0 0 "Business Administration" 18 3 2 1 1 1 1 1 0 0 1 1 1 3 2 1 2 1 0 0 0 0 1 0 0 2 2 2 2 1 2 101 101 101 101 101 101 20 25 6 5 5 5 5 5 5 4 4 5 5 5 3 3 3 4 4 4 2 3 3 3 3 4 4 3 4 1 19 1 2 2 3 3 3 3 3 1 1 1)

  ,(Ans 6 5 3 4 4 4 4 4 2 4 3 4 4 4 4 4 4 4 4 4 4 2 4 4 4 1 0 0 0 0 2 1 1 7 0 0 0 1 0 0 "Civil Engineering" 14 3 99 1 0 1 1 0 0 0 1 3 2 2 3 1 3 1 0 0 0 0 1 1 0 4 4 3 4 101 2 101 101 101 101 4 101 18 20 6 5 5 5 3 3 2 5 5 5 1 5 2 2 3 2 2 2 3 3 3 1 2 4 4 4 3 1 19 1 3 3 3 3 2 3 4 0 0 0)

  ,(Ans 6 6 1 2 4 4 4 2 1 4 3 2 2 2 2 2 2 2 3 3 2 1 2 2 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 1 0 0 0 0 1 6 2 1 2 1 3 1 1 0 0 0 0 1 0 4 3 3 4 3 2 4 3 3 3 3 2 6 45 6 5 5 5 5 5 5 5 5 5 5 5 2 2 2 3 2 3 2 2 3 2 2 4 2 3 4 1 19 1 1 1 2 2 2 4 3 0 0 0)

  ,(Ans 6 3 4 1 3 4 2 3 1 3 2 2 1 1 2 1 2 1 2 1 3 1 1 1 1 0 0 0 1 0 3 1 1 7 0 0 0 0 0 1 "English" 13 3 2 1 0 1 1 0 0 0 1 2 101 101 101 101 3 2 1 0 0 0 0 1 0 4 3 4 2 4 4 2 1 2 1 4 3 20 40 99 5 5 5 5 5 5 5 5 5 5 5 3 3 2 3 3 2 2 2 3 2 3 4 5 5 5 1 19 1 5 4 3 3 2 3 2 3 0 3)

  ,(Ans 6 3 3 1 4 3 101 2 3 3 3 3 3 4 4 2 3 3 3 3 4 4 2 2 3 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Business Administration" 12 3 2 1 1 1 1 1 0 0 1 2 101 101 101 101 3 3 1 0 0 0 0 0 0 3 3 3 2 3 4 2 2 2 2 2 4 101 101 6 5 5 5 5 5 5 5 5 4 5 4 4 4 3 4 4 4 4 4 3 4 3 3 4 4 3 0 19 101 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 3 5 3 1 4 3 3 3 1 3 3 2 4 4 3 1 3 1 3 2 3 1 1 4 3 1 0 0 0 0 4 1 1 7 0 1 1 0 0 0 "Foreign Languages" 14 3 2 1 0 1 1 0 0 0 1 6 3 1 1 2 3 3 1 0 0 0 1 0 0 3 4 4 3 3 3 3 3 3 2 3 2 101 101 99 3 4 3 2 3 5 5 3 4 1 2 2 3 2 4 4 2 5 2 2 2 3 4 5 2 5 1 19 1 2 2 3 3 4 2 3 1 0 1)

  ,(Ans 6 3 4 2 2 4 4 4 2 4 4 2 3 4 101 3 3 2 3 1 3 3 3 2 1 0 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "Business Administration" 14 3 2 1 1 1 1 1 0 0 1 1 1 3 2 1 1 1 0 0 0 0 0 0 0 4 1 3 2 3 3 3 1 3 1 3 2 20 18 3 5 5 4 4 5 3 5 5 5 5 5 3 4 3 5 5 4 4 4 4 4 5 4 3 3 3 1 19 1 2 3 3 4 3 3 3 1 2 1)

  ,(Ans 6 5 4 4 4 3 3 2 1 4 1 3 1 1 4 1 1 3 1 1 1 1 2 2 3 0 0 0 0 1 4 1 0 99 0 0 0 0 0 1 "Political Science" 15 3 101 1 0 1 0 0 0 0 1 6 3 1 1 2 1 99 1 1 0 1 1 0 0 4 3 2 3 4 4 2 1 4 2 1 1 0 6 1 1 3 2 5 3 1 1 1 3 0 5 2 2 2 2 2 2 2 2 2 2 2 3 4 4 5 1 19 1 2 1 3 4 3 2 3 1 1 1)

  ,(Ans 6 3 4 4 4 2 4 4 2 4 2 4 4 3 2 1 2 2 2 3 3 3 2 2 2 1 0 0 0 0 4 0 1 1 1 0 0 0 0 0 "Electrical Engineering" 15 3 1 1 0 1 1 0 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 1 0 3 3 3 3 2 2 3 3 3 3 1 1 101 101 6 5 3 5 3 4 3 3 3 4 4 5 3 2 4 3 3 3 2 2 2 3 3 5 3 4 5 1 19 1 3 5 3 1 1 1 1 0 0 0)

  ,(Ans 6 4 4 3 4 4 4 4 4 4 4 4 4 4 3 3 3 2 4 3 3 2 3 3 4 1 0 0 0 0 2 1 0 0 1 0 0 0 0 0 "Chemistry" 15 2 2 1 0 1 1 0 0 0 3 3 2 1 1 1 3 1 0 1 0 0 0 0 0 4 3 3 4 3 3 4 3 3 4 3 3 40 50 6 5 5 5 4 5 5 5 4 5 1 5 2 3 3 2 3 3 3 3 3 3 3 4 4 4 4 1 19 2 1 1 2 3 2 2 3 0 2 3)

  ,(Ans 6 3 4 4 4 4 4 3 2 3 3 3 4 3 2 2 3 4 3 3 2 1 2 3 4 0 0 0 0 1 4 1 1 2 0 1 0 0 0 0 "Health Studies" 14 2 1 1 0 0 0 0 0 0 1 6 2 1 3 2 2 2 1 0 0 0 0 0 0 3 4 4 3 3 4 3 4 4 2 2 4 25 25 6 5 4 5 5 5 4 5 4 4 4 5 3 3 3 2 3 3 4 3 4 4 5 4 5 3 5 1 19 1 1 1 3 3 3 3 3 0 0 0)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 4 1 4 4 4 4 1 4 3 3 4 1 4 4 1 1 0 0 0 2 1 1 0 1 1 1 0 1 0 "Biology" 14 2 1 0 0 1 1 1 0 0 1 6 1 2 1 1 3 1 1 1 1 0 0 0 0 1 1 1 1 1 3 101 101 101 101 101 101 3 30 6 0 5 5 5 5 5 5 5 5 5 5 1 1 2 5 5 5 3 1 5 5 5 5 5 5 5 1 19 2 99 99 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 3 3 4 3 4 4 3 4 4 3 3 4 4 3 4 3 3 3 4 3 4 3 4 1 1 0 0 0 4 1 0 0 1 1 1 0 0 0 "Business Administration" 14 2 2 1 1 0 1 1 0 0 1 3 2 2 3 1 3 1 1 0 0 0 0 0 0 4 4 3 4 4 4 4 4 3 3 3 3 13 20 6 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 1 19 1 2 1 3 3 3 3 3 1 0 1)

  ,(Ans 6 6 3 3 4 4 4 3 3 4 4 4 1 4 4 4 3 3 3 3 3 3 3 3 4 1 0 0 0 0 3 1 0 7 0 0 1 0 0 0 "Accounting" 15 2 2 1 0 0 0 0 0 0 1 2 1 1 1 1 3 1 0 0 0 0 1 0 0 3 2 2 2 2 3 101 101 101 101 101 101 5 20 6 2 2 2 1 1 1 1 1 1 1 3 3 3 3 1 2 4 3 2 3 3 3 4 2 4 3 0 19 1 2 2 3 3 2 3 3 0 0 1)

  ,(Ans 5 4 1 1 1 4 4 4 2 4 2 2 3 3 2 1 3 1 3 1 3 2 2 2 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Political Science" 14 2 99 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 1 2 3 3 4 3 3 3 3 3 101 101 6 5 5 5 5 3 4 4 5 5 3 2 3 3 4 4 4 3 3 4 4 4 4 4 4 3 3 1 19 1 5 5 3 3 2 2 2 0 0 1)

  ,(Ans 6 5 3 4 4 101 4 4 3 4 4 4 4 3 1 2 3 1 2 1 1 1 1 2 3 1 0 0 0 0 2 1 1 7 1 1 1 0 0 0 "Undecided" 13 2 1 1 0 0 0 0 0 0 1 6 3 101 101 101 2 3 1 1 0 0 0 0 0 4 4 4 3 3 4 4 4 4 2 3 4 101 101 5 2 3 3 2 5 5 5 2 2 5 5 2 2 2 3 3 3 1 3 2 3 2 4 4 4 4 1 19 2 2 2 3 4 3 3 2 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 2 2 3 2 2 4 3 2 3 3 1 3 2 3 3 2 2 2 1 0 0 0 0 3 1 1 4 0 0 0 1 0 0 "Science" 12 2 2 1 0 1 1 1 0 0 1 1 2 1 1 1 3 1 0 0 0 0 0 0 0 3 2 2 2 3 3 3 1 2 2 2 2 7 60 2 0 5 2 2 1 1 3 2 3 1 2 1 3 1 3 3 2 3 3 3 2 2 3 3 3 3 0 19 1 5 3 3 3 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 4 3 1 3 2 3 3 4 2 2 1 2 1 2 3 3 1 3 4 2 0 1 0 0 0 3 1 1 7 0 0 1 0 0 0 "International Studies" 14 2 101 1 0 1 1 1 0 0 1 6 3 1 1 2 2 3 1 1 0 0 1 1 0 3 4 3 3 3 3 4 3 3 1 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 4 3 4 2 3 3 3 5 4 4 4 1 19 1 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 3 3 3 3 4 4 4 3 2 4 4 3 3 4 3 4 4 1 3 3 4 4 3 2 4 1 1 1 0 0 3 1 1 0 1 0 1 0 1 0 "Architecture" 18 2 2 1 1 1 1 1 0 0 1 2 3 1 1 2 3 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 3 3 4 3 4 100 150 6 3 3 3 3 3 3 2 3 3 3 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 19 1 1 1 2 2 2 2 4 3 3 0)

  ,(Ans 6 3 2 2 4 3 3 4 2 4 3 4 4 4 3 1 2 1 4 4 4 4 3 3 4 1 0 0 0 0 4 1 0 7 0 1 1 1 0 0 "" 14 2 3 1 0 1 1 0 0 0 1 3 3 1 1 1 2 3 1 0 0 0 1 0 0 1 1 2 1 1 2 4 4 3 4 4 2 101 101 6 2 3 2 2 5 101 3 3 3 1 4 2 2 2 101 3 3 3 2 2 2 4 4 4 4 101 1 19 2 2 5 3 101 3 3 3 3 0 3)

  ,(Ans 6 101 3 1 2 4 4 4 3 3 4 3 3 4 1 1 2 1 2 3 4 3 3 4 4 1 1 0 0 0 5 1 1 7 0 0 0 0 0 0 "Did not answer" 17 2 99 1 0 1 0 0 0 0 1 6 3 1 1 1 1 2 1 1 0 0 0 0 0 4 3 3 3 3 2 3 2 2 1 3 1 1 5 3 5 5 5 4 3 2 2 3 4 4 5 5 5 5 5 4 4 5 3 5 5 5 5 5 5 5 1 19 2 5 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 4 4 2 1 3 4 4 4 4 3 3 3 2 2 3 3 1 1 4 3 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Foreign Languages" 12 2 2 1 0 1 0 0 0 1 1 6 3 101 101 101 3 1 0 0 0 0 0 0 0 4 4 4 4 3 3 3 3 3 3 3 3 30 90 6 2 5 2 3 5 4 5 1 2 3 4 2 2 1 4 3 3 2 2 2 2 2 4 3 2 3 1 19 1 1 2 1 4 2 2 3 1 1 1)

  ,(Ans 5 5 1 1 3 3 3 3 3 3 3 3 2 2 4 1 2 1 2 1 1 2 1 1 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Political Science" 12 2 2 1 0 1 1 1 0 0 1 1 3 1 1 2 2 3 0 0 0 0 1 0 0 4 2 2 2 3 3 4 1 1 1 2 3 101 101 6 3 5 3 5 5 2 5 5 3 5 5 4 4 4 5 5 3 5 5 5 4 3 4 4 2 2 99 19 1 5 5 3 4 2 3 3 3 3 3)

  ,(Ans 6 6 4 1 3 4 4 2 4 3 3 3 4 4 2 3 3 2 3 3 2 3 3 3 4 1 0 0 0 0 3 0 1 0 0 0 1 0 0 0 "Psychology" 14 2 2 0 0 1 1 1 0 0 1 3 2 2 2 1 3 1 0 1 0 0 0 0 0 3 4 4 4 4 4 3 4 3 3 4 4 101 25 6 2 3 1 4 2 1 1 1 3 4 5 2 3 2 5 4 3 2 2 3 3 3 4 4 4 4 1 19 2 2 1 3 3 2 3 3 1 1 1)

  ,(Ans 3 6 3 1 3 3 3 3 3 3 3 3 3 2 3 2 3 1 3 1 1 2 1 1 1 1 0 0 0 0 4 0 1 0 0 1 0 0 0 0 "Computer Science" 12 2 2 1 0 1 1 1 0 0 1 1 2 1 3 1 1 1 1 0 0 0 0 0 0 4 4 3 3 3 4 3 2 2 3 1 4 3 10 3 5 5 5 5 5 5 5 5 5 5 5 5 3 3 4 3 5 4 4 5 4 4 4 4 1 2 1 19 1 5 5 3 4 4 2 3 1 2 1)

  ,(Ans 6 4 4 4 4 4 4 4 4 1 3 4 4 4 4 4 4 1 4 3 2 4 3 4 3 1 1 0 0 0 5 1 1 0 0 0 1 0 0 0 "Architecture" 20 2 1 1 1 1 0 0 0 0 1 3 3 2 1 1 3 2 0 1 0 0 0 0 0 4 4 4 3 101 4 101 101 101 3 3 101 5 15 99 5 5 0 5 5 3 5 1 1 5 0 3 1 1 5 5 1 5 4 4 4 3 5 5 3 4 1 19 2 4 4 3 4 3 3 3 0 0 0)

  ,(Ans 6 4 4 1 4 3 3 4 3 3 3 2 4 4 1 2 2 1 3 1 1 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 1 0 1 "Did not answer" 14 2 2 1 0 1 1 1 0 0 1 1 2 2 1 3 2 3 1 0 0 0 0 0 0 4 2 2 2 1 3 4 3 2 2 1 3 101 101 6 5 5 5 5 5 5 5 3 5 2 3 4 4 3 4 4 5 4 3 4 3 4 3 4 3 4 1 19 1 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 5 1 1 7 0 0 1 0 0 0 "Music" 14 2 2 1 0 0 0 0 0 0 1 6 3 2 1 1 2 3 0 0 0 0 0 0 0 4 4 4 4 3 3 2 3 3 2 2 2 101 101 5 5 5 2 5 5 3 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3 3 1 19 1 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 3 5 3 2 4 3 3 3 1 3 4 3 3 4 2 3 3 2 3 2 3 1 4 4 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 12 2 2 1 0 1 0 0 0 0 1 1 1 3 1 1 2 1 0 0 0 0 0 0 0 3 2 2 3 4 3 3 2 3 2 3 2 22 30 6 1 5 5 3 5 5 5 3 2 5 2 4 5 4 5 5 3 4 3 2 2 5 4 4 3 4 1 19 1 3 3 3 4 3 3 3 3 0 0)

  ,(Ans 3 5 3 1 3 4 4 3 2 4 4 1 4 3 1 1 1 1 3 2 4 3 1 3 1 1 0 0 0 1 4 1 1 99 0 0 0 0 0 1 "Engineering" 15 2 1 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 1 1 0 0 0 1 1 4 2 2 2 3 4 3 2 2 1 2 4 101 101 6 5 5 101 5 5 5 5 5 5 5 3 3 2 2 4 4 5 5 4 4 4 5 2 3 1 4 1 19 1 5 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 3 3 2 2 4 3 3 4 4 3 4 2 1 2 2 2 2 3 2 2 0 0 0 0 1 2 0 1 0 0 0 1 0 0 0 "Undecided" 12 2 2 1 0 1 0 0 0 0 1 1 1 2 1 1 3 1 1 0 0 0 0 0 0 3 2 2 3 3 2 4 2 2 3 3 2 101 60 6 5 5 4 2 1 1 5 3 5 1 5 3 4 5 3 3 2 3 3 3 3 4 4 3 4 3 1 19 1 4 99 3 4 2 3 3 3 2 3)

  ,(Ans 6 3 3 4 3 4 4 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 14 2 2 1 0 1 0 1 0 0 1 6 1 1 2 2 2 1 1 0 0 0 0 0 0 3 3 2 2 2 2 3 3 3 3 3 3 2 10 6 3 3 2 5 2 5 5 2 3 1 5 2 2 2 4 4 4 2 2 3 1 3 4 4 5 5 1 19 1 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 5 4 2 3 4 4 4 4 2 3 2 3 4 3 3 3 3 2 2 2 3 3 3 3 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Civil Engineering" 13 2 2 1 0 0 0 0 0 0 1 6 2 1 2 2 2 3 1 1 0 0 1 0 0 3 3 3 3 3 3 3 2 2 2 3 3 101 101 5 5 3 3 3 2 3 3 3 2 1 5 3 3 3 3 3 3 3 4 4 4 4 4 5 4 3 1 19 1 4 3 3 2 3 3 3 3 2 3)

  ,(Ans 6 6 4 4 4 4 4 3 1 3 2 4 2 3 1 1 1 1 3 1 1 3 1 1 1 0 1 0 0 0 4 0 1 99 0 0 0 0 0 1 "Engineering" 14 2 99 1 0 0 0 0 0 1 1 1 1 2 2 2 3 1 0 0 0 0 0 0 0 4 4 3 3 3 3 3 2 3 2 2 1 24 30 6 5 5 5 3 5 4 5 4 5 5 5 3 4 3 4 4 4 3 3 4 3 4 4 4 3 3 1 19 1 3 4 3 3 4 2 2 1 2 1)

  ,(Ans 6 6 2 3 3 4 4 4 4 4 4 3 4 3 2 2 3 2 2 2 3 3 3 3 1 1 0 0 0 0 2 1 1 0 1 0 0 0 0 0 "Did not answer" 14 2 1 1 0 1 1 1 0 0 1 1 3 1 1 1 3 3 1 0 0 0 0 0 0 4 3 3 2 3 3 4 4 3 2 2 3 101 101 6 5 5 5 5 5 5 5 5 5 4 5 4 4 3 3 4 2 3 2 4 3 4 4 5 5 4 1 19 1 4 4 2 4 3 3 3 3 0 3)

  ,(Ans 6 6 4 1 1 3 3 4 2 1 2 4 2 4 4 3 3 1 3 3 2 3 2 3 4 1 0 0 0 0 3 1 1 0 0 1 0 0 0 0 "Biology" 14 2 2 1 0 1 1 1 0 0 1 3 1 3 2 1 2 1 1 1 0 0 1 0 0 1 1 1 1 1 1 101 101 101 101 101 101 14 30 6 5 5 5 5 5 5 3 5 5 2 5 3 3 3 3 3 5 3 3 3 2 5 3 3 3 3 1 19 1 3 3 3 3 3 3 2 3 2 3)

  ,(Ans 3 5 4 2 4 4 4 4 2 4 4 4 3 3 4 3 3 3 3 4 4 3 3 3 4 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Business Administration" 12 2 2 1 0 1 0 0 0 0 1 3 1 3 1 1 1 2 0 0 0 0 0 1 0 4 3 3 3 4 4 3 3 2 2 3 3 6 15 6 3 4 2 4 4 3 4 5 3 4 3 3 4 5 3 2 2 2 3 4 4 4 2 3 5 4 0 19 1 2 2 3 3 2 3 3 2 2 2)

  ,(Ans 6 6 4 1 3 3 3 4 3 3 3 3 3 3 2 4 2 1 3 2 3 3 3 3 4 0 0 0 0 1 3 1 1 0 0 0 0 0 1 0 "Biology" 15 2 1 1 1 1 0 0 0 0 1 3 1 2 3 1 1 1 1 1 0 0 0 0 0 3 3 3 3 3 3 3 2 2 2 2 2 15 20 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 3 3 5 3 3 4 3 4 3 4 4 4 1 19 2 2 3 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 3 2 4 4 4 2 2 2 1 4 4 3 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 2 1 0 0 0 0 0 0 0 1 "Civil Engineering" 12 2 99 1 0 1 1 0 0 0 1 6 3 1 1 2 1 3 0 0 0 0 0 1 0 4 1 3 1 4 3 4 1 3 1 4 3 101 101 6 5 5 3 1 5 5 3 1 5 5 1 3 3 2 4 4 4 2 2 4 4 4 1 5 1 1 1 19 1 3 4 3 3 4 3 4 3 2 3)

  ,(Ans 6 6 3 1 2 3 4 2 4 2 3 4 3 2 2 1 1 1 1 1 3 2 3 3 4 0 1 0 0 0 3 1 1 0 0 0 1 0 0 0 "English" 18 2 2 1 0 1 1 1 0 0 1 3 1 1 2 2 3 2 1 0 0 0 0 0 0 4 3 3 2 3 4 3 3 3 2 3 4 6 15 6 4 4 4 1 0 0 0 5 4 5 4 5 4 4 5 5 5 5 5 5 5 4 5 5 5 5 1 19 1 2 4 3 4 3 3 3 1 2 1)

  ,(Ans 6 3 4 2 4 3 3 3 2 4 3 3 4 4 4 1 2 1 3 1 2 1 1 4 4 1 0 0 0 0 3 1 1 7 1 1 1 0 0 0 "Foreign Languages" 15 2 3 1 0 1 1 1 0 0 1 6 3 1 1 2 1 3 1 0 0 0 0 0 1 3 3 3 3 3 3 3 3 3 2 4 3 101 101 6 5 5 5 3 5 5 4 5 4 1 2 3 2 3 3 3 3 2 3 3 2 3 3 3 2 2 0 19 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 2 4 1 4 4 4 4 2 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 0 1 0 3 1 1 7 0 1 1 1 0 0 "Did not answer" 15 2 2 1 1 1 1 1 0 0 1 3 1 2 2 1 1 2 1 0 1 0 0 0 0 3 2 2 3 3 1 101 101 101 101 101 101 10 15 6 2 2 3 3 2 3 3 3 3 3 1 3 3 4 4 4 3 4 3 4 4 4 4 4 4 4 1 19 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 3 6 3 1 4 4 4 2 4 4 4 3 4 2 3 1 4 1 2 1 4 4 4 4 3 1 0 0 0 0 4 1 1 1 0 0 1 0 0 0 "Undecided" 12 2 2 1 0 1 1 1 0 0 1 6 3 1 1 2 1 3 0 0 0 0 0 0 1 4 4 4 2 3 4 4 3 3 1 3 4 101 101 6 5 3 5 5 5 5 5 4 5 5 4 4 4 5 5 5 5 3 3 4 5 5 5 5 4 5 1 19 2 3 5 3 3 4 3 3 3 2 3)

  ,(Ans 6 6 3 3 3 4 4 3 4 4 3 3 4 3 3 1 2 2 2 1 2 1 1 2 2 1 0 0 0 0 4 1 0 0 1 1 1 0 0 0 "Business Administration" 15 2 2 1 0 1 0 1 0 0 1 1 3 101 101 101 101 3 0 0 0 0 1 0 0 4 4 4 4 4 4 4 3 3 2 2 2 101 101 6 5 5 5 5 5 4 3 3 4 0 1 2 3 3 5 5 2 4 2 2 2 3 3 3 4 4 1 19 1 5 3 3 4 3 3 3 3 2 3)

  ,(Ans 3 6 3 3 3 4 4 2 3 1 4 3 4 3 3 1 3 1 3 1 3 1 1 3 2 1 1 0 0 0 4 1 0 99 0 1 1 0 0 0 "Undecided" 14 2 1 1 1 1 1 1 0 0 1 1 3 1 2 2 2 3 1 1 0 0 0 1 0 101 3 3 101 101 4 3 101 3 2 3 101 101 101 6 5 5 5 4 4 5 5 5 5 5 3 3 3 3 5 5 4 3 5 5 4 4 4 4 4 3 1 19 1 5 5 3 4 4 3 2 3 0 3)

  ,(Ans 6 6 3 1 4 4 4 1 1 4 4 4 4 3 3 1 2 1 2 1 1 1 1 1 4 1 0 1 0 0 3 1 1 0 1 0 1 0 0 0 "Undecided" 12 2 2 1 0 1 1 1 0 0 1 1 3 101 101 2 2 3 0 0 0 0 1 1 1 4 4 2 2 3 2 3 4 2 2 3 2 101 101 6 3 3 3 1 5 4 5 0 1 4 5 3 4 3 5 5 4 2 2 3 4 4 4 3 99 99 1 19 1 3 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 2 4 2 4 4 4 4 3 4 4 4 3 3 3 3 4 3 4 4 4 3 3 3 4 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "Biology" 14 2 2 1 0 1 0 0 0 0 3 3 101 101 3 101 3 1 1 0 0 0 0 0 0 101 101 101 101 101 101 4 4 4 4 3 4 35 35 6 5 5 3 5 5 5 5 5 5 5 5 1 2 2 2 2 2 1 1 1 1 1 5 4 4 3 1 19 2 2 2 3 3 3 3 3 3 3 3)

  ,(Ans 3 6 3 1 4 4 4 1 3 2 4 4 3 3 3 1 2 1 3 4 4 1 1 4 4 1 0 0 0 0 3 101 1 0 0 1 1 1 0 0 "Undecided" 13 2 2 1 0 1 1 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 3 3 2 2 3 3 1 3 1 1 1 101 101 5 5 5 1 1 3 0 3 0 2 5 1 3 4 1 5 5 2 3 3 4 4 5 4 4 3 3 0 19 2 5 5 3 4 4 2 3 3 2 3)

  ,(Ans 3 4 4 1 3 4 4 4 1 1 4 4 2 2 2 1 4 1 4 2 3 2 4 4 4 1 1 0 0 0 4 1 1 0 0 0 0 0 0 0 "Philosophy" 13 2 99 0 0 0 0 0 0 1 1 2 3 101 101 2 101 3 1 1 0 0 1 0 1 4 4 4 1 4 4 4 4 4 1 4 4 101 101 6 3 5 5 5 5 5 5 4 5 5 5 4 5 3 3 3 5 5 3 5 5 5 5 5 5 5 1 19 1 4 3 3 4 1 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 4 4 3 1 4 1 2 3 1 4 2 3 1 3 3 4 0 0 0 0 1 5 0 1 99 0 0 0 0 0 1 "Business Administration" 20 2 2 1 0 1 1 1 0 0 101 2 1 2 2 1 3 1 0 0 0 0 0 0 0 3 2 4 1 3 3 3 1 3 1 1 2 10 50 3 5 5 5 0 2 2 5 5 5 2 5 4 5 5 4 4 3 3 4 5 5 5 5 4 3 4 1 19 2 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 4 4 3 4 3 2 4 3 4 4 4 4 4 3 3 3 3 4 1 1 3 4 1 0 0 1 0 4 1 1 7 0 1 0 0 0 0 "Psychology" 13 2 2 1 0 1 1 1 0 0 1 2 1 3 2 1 1 1 1 0 0 0 1 0 0 4 3 3 2 3 4 4 3 3 2 3 3 30 20 6 3 5 4 5 5 2 5 2 5 0 5 4 5 5 5 5 3 3 4 5 3 5 4 5 99 99 1 19 1 2 2 3 3 2 3 3 1 0 1)

  ,(Ans 3 2 2 1 4 2 2 4 2 2 1 2 3 1 2 4 2 1 2 1 1 1 1 1 1 1 0 0 0 0 4 0 1 0 0 0 0 0 0 0 "" 15 2 2 1 0 0 0 0 0 0 1 1 1 2 3 1 3 1 0 1 0 0 0 0 0 3 3 2 1 4 2 1 1 1 1 3 1 5 15 1 5 5 3 3 5 1 3 1 3 3 1 5 5 2 1 2 3 3 3 4 2 5 3 3 3 2 0 19 1 2 5 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 3 1 4 4 4 3 1 2 3 3 4 3 3 1 2 1 1 1 1 1 1 2 1 0 0 0 0 1 4 1 1 0 0 0 0 0 0 1 "Music" 13 2 1 1 0 0 0 0 0 0 1 1 3 2 2 1 3 3 1 0 0 0 0 0 0 4 4 4 4 4 4 2 2 2 2 2 2 101 101 99 3 3 3 3 3 3 4 3 2 3 3 3 3 3 3 3 3 5 3 4 2 3 3 3 3 3 1 19 1 4 2 3 3 3 3 3 0 0 3)

  ,(Ans 6 6 3 3 3 3 1 1 1 2 4 2 2 2 1 2 2 1 2 1 2 3 1 4 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Music" 12 2 99 1 0 1 1 1 0 0 1 6 2 2 1 1 3 2 0 0 0 0 0 1 0 4 2 3 1 2 4 4 2 3 1 2 4 5 20 3 5 5 4 5 5 2 3 1 1 4 5 2 1 1 5 5 2 2 2 2 3 2 4 5 3 3 0 19 1 2 3 3 4 4 3 3 0 2 0)

  ,(Ans 6 4 4 4 4 4 4 4 4 4 4 4 3 4 4 4 4 2 4 1 2 2 2 2 4 1 1 1 0 0 4 1 1 7 0 0 1 0 0 0 "Administration of Justice" 9 2 2 1 0 1 1 1 0 0 1 6 1 3 1 1 2 1 1 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 19 30 99 2 2 2 5 5 2 2 2 2 2 2 3 3 3 4 4 4 4 3 3 3 3 4 4 4 4 0 19 2 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 3 4 4 4 4 3 3 4 4 4 4 4 3 4 3 1 3 4 3 2 2 4 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Applied Linguistics" 13 2 2 1 0 1 1 0 0 1 1 1 1 1 1 3 2 1 0 0 0 0 0 0 0 4 3 2 2 4 3 3 2 1 1 3 2 5 15 6 1 5 5 5 4 5 5 5 5 3 3 2 3 3 5 5 5 5 3 3 3 4 3 4 3 4 1 19 1 4 3 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 4 4 4 4 4 3 4 4 3 4 4 4 1 4 4 1 4 2 2 3 1 1 2 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Civil Engineering" 14 2 2 1 0 1 0 1 0 0 1 6 2 2 2 2 3 1 1 0 0 0 0 0 0 4 3 3 3 3 3 3 1 2 2 3 2 10 30 6 5 5 5 0 5 5 5 4 3 0 2 4 4 4 3 4 4 4 4 3 3 3 3 4 3 3 1 19 1 3 5 3 3 3 3 3 3 0 3)

  ,(Ans 5 5 3 2 3 4 4 4 2 3 4 4 3 4 4 3 3 3 3 4 3 3 3 3 2 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Business Administration" 13 2 2 1 0 1 0 0 0 0 1 1 101 3 101 101 101 1 0 0 0 0 1 0 0 3 4 101 2 3 2 3 4 101 2 3 2 10 15 6 5 5 3 2 5 5 4 1 5 3 3 4 4 4 5 5 4 4 3 5 4 5 4 4 4 5 1 19 1 3 5 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 1 3 3 2 2 2 4 3 1 4 4 2 3 2 4 3 3 1 2 3 2 4 0 0 0 0 1 3 1 1 0 0 0 0 0 0 0 "Did not answer" 13 2 3 1 0 1 0 1 0 0 1 3 1 1 1 1 1 3 0 0 0 0 0 0 0 3 2 2 1 2 3 2 1 1 1 2 2 101 101 6 5 5 4 4 3 0 3 0 5 5 2 4 3 4 5 5 3 4 2 4 4 4 3 4 3 3 1 19 2 5 5 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 4 1 3 3 4 1 4 4 3 3 2 2 101 1 3 1 2 2 2 4 2 3 1 1 0 0 0 0 4 0 1 0 1 0 1 0 0 0 "International Studies" 14 2 2 1 0 1 0 1 0 0 1 1 1 1 2 1 3 2 0 0 0 0 0 0 0 4 4 3 2 4 4 4 2 1 1 4 2 5 20 4 101 5 5 1 1 3 3 0 4 5 5 1 4 4 5 5 5 4 2 3 4 4 3 5 3 4 0 19 2 5 5 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 1 4 4 4 4 1 4 4 3 4 3 3 3 2 3 3 3 3 1 3 3 1 0 0 1 0 5 1 1 0 0 0 0 1 0 0 "Business Administration" 15 2 101 1 0 0 0 0 0 0 1 1 101 101 101 101 3 1 1 0 0 0 0 0 0 4 3 2 2 2 3 101 101 101 101 101 101 2 10 6 5 5 5 1 5 3 5 4 4 3 5 3 3 3 3 3 3 3 3 3 3 3 4 3 3 4 1 19 1 1 99 3 3 4 2 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 4 2 3 4 3 1 1 2 1 1 2 4 2 2 1 1 1 1 4 0 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Administration of Justice" 12 2 2 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 3 1 4 3 4 3 2 1 2 101 101 6 3 5 3 2 4 1 3 3 2 2 5 2 2 3 5 5 3 1 4 3 2 3 3 3 3 3 0 19 1 2 4 3 3 4 2 3 3 0 3)

  ,(Ans 6 2 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 1 0 "Did not answer" 18 1 101 1 0 1 1 0 0 0 1 1 1 3 2 1 2 1 0 1 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 10 10 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 19 1 3 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 2 1 1 4 4 4 4 3 4 4 1 1 4 4 1 4 4 4 4 3 1 3 3 2 0 0 0 1 0 2 1 1 0 0 0 0 1 0 1 "Did not answer" 16 1 101 1 0 1 0 0 0 1 1 6 3 1 1 1 1 3 0 0 0 0 0 1 1 4 4 2 3 3 3 3 4 3 2 3 3 101 101 6 1 4 1 5 4 3 4 3 1 5 0 2 1 2 4 5 3 5 3 4 5 3 5 5 4 5 1 19 1 2 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 3 1 4 3 3 3 3 4 4 2 4 3 3 1 2 1 2 1 2 1 1 2 2 1 0 0 0 0 3 0 0 0 0 0 1 1 0 0 "Undecided" 12 1 101 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 1 0 0 0 3 3 3 2 3 2 3 2 2 2 3 2 101 101 6 3 5 1 5 4 3 5 0 3 4 5 3 4 3 3 3 3 4 3 3 3 3 3 4 4 4 1 19 2 4 3 3 4 4 3 3 1 0 1)

  ,(Ans 6 4 4 1 4 4 3 4 2 4 2 3 4 4 4 1 3 1 3 3 3 2 2 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Chemistry" 15 1 101 1 0 1 1 0 0 0 1 2 3 101 101 101 3 2 0 0 0 0 0 1 0 4 3 4 1 3 3 101 101 101 101 101 101 15 30 6 3 3 0 2 2 2 1 2 2 5 5 1 1 1 3 3 3 1 3 3 3 4 4 5 3 3 0 19 1 2 4 3 3 4 3 3 3 2 3)

  ,(Ans 3 6 4 3 3 101 4 3 3 2 4 3 3 4 3 1 3 3 3 3 3 3 1 3 1 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Art" 12 1 99 1 0 1 1 1 0 0 1 1 3 1 1 3 2 3 1 0 0 0 0 1 0 4 4 4 3 2 4 101 101 101 101 101 101 101 101 6 5 5 3 4 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 19 2 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 4 4 4 4 4 1 2 4 4 1 1 1 3 1 3 4 3 3 2 2 3 1 4 1 0 0 1 0 4 1 1 0 0 1 0 0 0 0 "Administration of Justice" 17 1 99 1 0 0 0 0 0 0 1 3 3 1 2 1 2 3 1 0 0 0 0 0 0 1 1 1 1 1 1 2 2 2 2 2 2 101 101 3 4 5 3 5 5 2 1 0 3 4 4 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 1 19 1 2 99 3 1 4 3 3 3 0 0)

  ,(Ans 6 5 3 4 3 4 4 4 3 3 3 3 4 4 3 1 3 1 3 1 1 3 2 3 2 1 0 0 0 0 5 0 0 0 0 0 0 0 0 1 "Theater Arts" 16 1 101 1 0 1 1 0 0 0 1 1 3 1 1 2 2 3 0 0 0 0 0 1 0 3 2 2 2 2 3 2 2 1 1 1 2 101 101 99 3 3 3 2 4 2 4 3 2 3 1 4 3 2 2 3 3 5 5 4 5 3 5 5 3 3 1 19 1 4 5 3 2 3 3 3 3 0 3)

  ,(Ans 6 6 3 1 1 2 2 4 2 2 3 3 2 2 1 3 2 1 2 1 2 3 2 1 2 1 0 0 0 0 4 1 0 2 0 0 0 0 1 0 "Biology" 16 1 101 1 0 0 0 0 0 0 1 6 101 101 3 2 2 2 0 0 0 0 1 0 0 3 4 2 3 2 3 3 3 2 2 1 2 4 6 3 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 5 5 2 4 4 3 4 3 3 5 3 1 19 1 2 3 3 4 4 4 2 3 0 3)

  ,(Ans 5 3 3 4 4 4 4 4 3 4 4 4 4 2 3 3 3 1 3 2 3 3 4 3 3 1 0 0 0 0 2 0 1 1 1 0 1 0 0 0 "Art" 101 1 101 1 0 0 0 1 0 0 1 1 3 1 1 3 2 3 0 0 0 0 1 1 0 4 3 3 4 4 4 4 3 3 4 4 4 101 101 6 4 5 5 1 5 4 4 5 5 3 5 4 3 3 5 5 5 4 3 4 2 2 5 5 4 3 1 19 1 4 4 101 101 101 101 101 101 101 101)

  ,(Ans 5 6 3 4 4 4 4 4 3 4 3 4 4 3 3 3 4 2 2 3 3 4 4 3 3 1 0 0 0 0 2 0 1 1 1 0 1 0 0 0 "Art" 101 1 101 1 0 0 0 1 0 0 1 1 3 1 1 3 2 3 1 0 0 0 0 1 0 4 3 4 4 3 4 4 3 4 3 4 4 101 101 6 5 5 4 2 4 2 4 3 5 4 5 5 5 4 4 5 4 4 5 5 5 5 5 5 4 5 1 19 1 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 2 2 4 3 2 1 3 3 4 3 1 3 1 1 1 1 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Computer Science" 15 1 99 1 0 1 1 1 0 0 1 6 3 1 2 2 3 2 0 0 0 0 0 0 0 3 3 2 1 3 3 3 2 1 1 3 3 101 30 5 5 4 5 2 2 1 2 0 2 2 2 3 3 2 4 4 2 2 2 4 3 4 3 3 3 3 0 19 1 1 1 2 3 4 3 3 1 1 1)

  ,(Ans 5 5 1 1 4 4 4 2 3 4 3 3 4 1 1 1 1 4 1 1 1 1 1 2 1 1 0 0 0 0 3 0 1 0 0 0 0 0 0 0 "Undecided" 13 1 101 1 0 0 0 0 0 0 1 6 1 3 1 1 1 2 0 0 0 0 0 0 0 4 4 3 2 3 1 4 4 101 101 101 101 2 8 6 5 5 5 5 3 3 5 3 3 2 5 3 3 3 3 4 4 3 3 3 2 3 3 3 3 3 1 19 1 2 2 3 3 2 3 2 3 0 3)

  ,(Ans 6 1 4 1 1 1 4 1 101 4 3 3 4 1 3 1 1 1 1 1 1 1 1 2 2 1 0 0 0 0 3 1 1 101 0 0 0 0 0 0 "Civil Engineering" 14 1 101 1 0 1 0 0 0 0 1 3 3 1 1 2 1 3 0 0 0 0 1 0 0 4 2 2 2 3 4 3 1 2 1 3 3 101 101 6 5 5 4 3 3 5 4 3 3 0 4 4 3 2 3 3 4 2 4 2 2 2 3 4 3 4 1 19 1 4 3 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 3 3 4 3 3 4 3 4 3 4 4 3 3 3 3 3 3 4 3 4 3 3 1 0 0 0 0 3 1 1 7 1 0 1 0 0 0 "Accounting" 14 1 101 1 0 1 0 0 0 0 1 3 3 101 101 101 3 2 1 0 0 0 0 0 0 4 4 4 3 4 4 4 4 4 2 4 3 50 90 6 5 5 3 0 2 0 4 1 1 0 1 2 2 3 3 3 2 3 2 3 2 3 3 3 2 2 1 19 2 2 4 3 3 4 3 4 2 2 2)

  ,(Ans 6 3 4 3 4 4 4 4 3 4 3 3 4 3 3 1 2 2 3 2 4 3 2 4 1 1 0 0 0 0 3 0 0 0 0 0 1 0 0 0 "Anthropology" 5 1 101 1 0 0 0 0 0 0 1 3 3 1 1 1 3 3 0 1 0 1 0 0 1 101 101 3 3 3 101 3 3 101 101 101 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 4 5 5 5 5 5 5 5 101 5 5 1 19 2 5 3 3 4 4 3 3 1 1 1)

  ,(Ans 3 6 2 2 3 3 4 3 3 3 1 3 2 2 2 4 1 1 2 2 2 2 2 2 2 0 1 0 0 0 5 1 1 0 0 0 0 0 0 1 "Health Studies" 12 1 101 1 0 1 1 1 0 0 1 3 101 2 101 101 3 1 0 0 0 0 0 0 0 101 3 3 101 3 101 101 3 101 101 3 101 15 30 6 4 4 4 0 0 0 0 0 2 0 4 4 3 4 3 3 3 3 2 3 3 3 4 4 99 99 1 19 1 3 3 3 4 4 3 3 1 0 1)

  ,(Ans 6 5 3 4 4 4 4 4 4 4 4 3 3 3 3 3 2 3 3 3 3 3 3 3 3 1 1 0 0 0 4 0 1 7 1 1 1 0 0 0 "Business Administration" 14 1 99 1 0 1 0 0 0 1 1 3 3 1 1 1 1 3 1 1 0 0 0 0 1 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 2 2 2 5 5 5 5 5 2 4 3 3 3 2 2 2 2 2 2 2 2 2 3 4 3 3 1 19 1 2 1 1 1 1 3 3 3 0 3)

  ,(Ans 6 6 4 101 4 4 4 3 1 3 3 4 3 4 3 4 3 2 3 3 3 3 2 2 4 1 0 0 0 0 3 1 0 7 1 0 0 1 0 0 "Electrical Engineering" 14 1 101 0 0 1 1 1 0 0 1 6 2 2 2 1 3 1 1 1 0 0 0 1 0 4 4 3 3 3 4 4 3 3 2 3 4 101 45 6 5 5 5 5 5 5 5 5 5 5 5 4 4 2 2 3 3 3 2 3 3 3 4 5 4 4 1 19 2 3 2 3 3 4 2 3 1 2 1)

  ,(Ans 6 2 4 3 3 3 3 4 2 3 4 3 2 3 2 3 3 1 3 2 3 2 2 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Biology" 4 1 101 1 0 0 0 0 0 0 1 1 1 3 2 1 2 2 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 2 2 3 2 101 45 6 5 5 5 3 5 3 5 4 4 4 4 5 5 5 5 5 5 5 5 4 4 4 4 4 3 4 1 19 2 2 2 3 3 4 3 3 1 1 1)

  ,(Ans 6 5 4 2 4 3 4 3 4 3 2 3 1 4 4 1 2 1 4 101 101 4 3 2 101 1 0 0 0 0 3 0 101 7 1 1 1 0 0 0 "Undecided" 101 1 101 0 0 1 1 1 0 0 1 3 101 101 101 101 101 1 0 0 0 0 0 0 0 4 3 3 101 4 4 4 101 101 101 4 4 101 45 6 0 5 1 4 2 101 101 101 101 101 101 101 3 2 101 4 3 101 101 101 101 101 3 4 4 4 1 19 2 1 3 101 101 101 101 101 0 0 101)

  ,(Ans 6 6 4 1 3 4 4 2 1 4 3 4 2 4 2 3 3 1 3 2 3 1 1 1 4 1 0 0 0 0 2 0 1 0 0 1 0 0 0 0 "Business Administration" 15 1 101 1 0 1 0 1 0 0 1 3 2 101 101 101 3 1 0 0 0 0 1 0 0 3 3 2 3 3 2 3 3 2 3 3 2 4 30 6 5 5 5 1 5 1 5 5 5 0 2 2 3 1 3 4 2 3 2 3 2 2 4 3 2 2 0 19 1 1 1 3 101 101 3 3 3 0 3)

  ,(Ans 6 6 101 3 4 3 3 3 2 4 3 3 3 4 3 2 2 2 3 3 2 2 1 2 3 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "Did not answer" 13 1 101 1 0 1 1 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 3 3 3 3 4 4 3 3 3 3 101 101 6 2 4 0 0 2 1 2 0 1 0 1 1 3 2 2 3 2 2 1 2 2 101 3 3 3 3 0 19 1 3 2 3 3 1 3 1 3 0 3)

  ,(Ans 6 3 4 2 4 4 4 4 4 4 4 3 3 2 2 1 1 2 4 1 1 1 2 2 2 1 1 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 16 1 2 1 0 1 0 0 0 0 1 1 2 1 1 3 3 3 1 0 0 0 0 0 1 4 3 3 2 3 3 4 3 3 1 1 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 5 4 4 4 4 5 5 5 5 1 19 2 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 3 3 4 2 3 3 4 3 4 3 1 2 4 3 3 3 3 2 3 4 0 0 0 0 1 3 0 1 99 1 0 0 0 0 0 "Business Administration" 15 1 101 1 0 1 1 0 0 0 1 3 3 1 2 1 3 3 0 0 0 0 1 0 0 4 4 3 2 3 2 101 101 3 101 101 3 101 101 6 2 2 1 0 0 0 2 1 0 4 3 2 1 2 3 1 2 1 3 2 3 3 4 4 101 4 1 19 1 3 3 3 3 4 2 3 0 0 1)

  ,(Ans 6 3 3 2 3 4 3 4 3 4 3 2 3 4 4 1 4 1 4 3 3 3 2 3 2 0 1 0 0 0 4 1 0 0 0 0 0 0 0 1 "International Studies" 13 1 101 0 0 0 0 1 0 0 1 3 3 1 1 1 2 2 1 1 0 0 0 1 0 4 4 4 2 2 3 3 3 4 2 2 3 4 20 99 1 1 1 1 2 1 1 0 2 1 3 1 2 1 2 1 1 1 1 2 3 3 4 4 4 3 1 19 2 4 4 3 2 4 3 2 0 0 0)

  ,(Ans 6 5 4 1 4 4 3 3 3 4 4 2 2 3 4 1 2 1 3 3 1 2 2 3 3 1 0 0 0 0 5 0 1 0 0 0 0 1 0 0 "International Studies" 16 1 101 1 0 1 1 1 0 0 1 6 3 1 1 2 1 3 1 0 0 0 0 0 1 4 4 4 2 4 4 3 3 101 1 2 2 101 101 6 5 5 5 5 5 3 5 3 5 4 4 4 3 4 5 5 4 4 4 5 3 5 5 5 3 3 1 19 2 4 3 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 4 2 4 4 4 1 2 1 4 4 4 2 1 4 1 1 0 0 0 0 4 1 1 0 1 0 0 1 0 0 "Foreign Languages" 14 1 101 1 0 0 0 1 0 0 1 1 3 1 2 1 1 3 1 0 0 0 0 0 0 3 3 3 3 3 3 4 3 2 2 2 2 101 101 6 5 5 101 2 4 5 5 4 5 5 3 5 5 3 2 3 5 3 4 5 5 3 3 3 4 4 0 19 2 3 5 3 3 2 2 2 1 0 1)

  ,(Ans 6 5 3 1 4 4 4 3 1 3 3 2 3 3 3 1 2 1 2 3 3 2 2 3 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "International Studies" 13 1 101 1 1 1 1 1 0 0 1 3 2 1 101 3 1 3 1 0 0 0 1 0 0 4 4 4 3 3 4 4 4 3 2 4 3 101 101 6 5 5 5 4 5 4 4 5 4 4 5 3 4 5 5 5 4 5 5 5 4 4 3 5 3 3 1 19 1 4 4 3 4 3 3 3 1 2 1)

  ,(Ans 3 5 3 2 2 3 2 2 3 2 2 2 4 2 2 1 2 1 2 1 3 3 3 3 2 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Art" 15 1 101 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 1 0 0 0 0 0 4 2 3 1 2 3 4 2 2 1 1 2 101 101 6 1 5 1 5 5 2 4 5 5 5 5 2 2 1 5 4 2 2 4 3 5 2 4 5 5 5 1 19 2 5 4 3 4 2 3 3 1 1 1)

  ,(Ans 6 5 3 1 3 3 3 2 2 3 3 3 101 2 1 3 2 3 2 2 1 1 2 3 2 0 0 0 1 0 3 0 1 7 0 0 1 0 0 0 "Undecided" 13 1 101 1 0 0 0 0 0 0 1 2 1 3 2 1 2 3 1 0 0 0 0 0 0 4 3 3 3 3 3 3 3 3 3 3 3 101 101 6 3 4 4 2 4 3 5 2 4 4 3 3 3 101 4 4 3 2 2 3 2 3 4 4 3 3 1 19 2 3 3 3 4 4 3 3 1 0 1)

  ,(Ans 6 6 4 2 2 2 2 4 1 1 1 1 4 3 3 1 1 1 3 3 3 4 2 2 1 0 0 0 1 0 3 0 0 2 1 1 1 1 1 0 "Did not answer" 15 1 101 1 0 0 0 0 0 0 1 3 3 1 1 2 2 2 1 0 0 0 0 1 0 3 3 3 4 3 2 101 101 101 101 101 101 20 20 6 0 3 3 3 0 4 3 0 0 2 2 1 1 1 1 2 101 1 1 1 1 1 5 5 4 5 1 19 1 2 4 3 1 1 3 3 0 0 0)

  ,(Ans 6 5 4 1 1 4 4 4 4 4 1 2 2 3 4 3 2 1 3 1 1 1 2 1 1 1 0 0 0 0 5 0 1 1 0 0 0 0 0 1 "Biology" 18 1 101 1 0 0 0 0 0 0 1 101 3 101 2 101 2 3 0 0 0 0 0 0 0 4 4 101 4 101 4 4 4 101 4 101 4 101 101 6 5 5 5 5 5 0 5 5 5 0 5 3 4 4 4 4 4 4 4 4 2 4 4 4 3 4 1 19 2 4 4 3 3 2 3 3 1 2 1)

  ,(Ans 6 3 4 1 1 4 4 4 3 4 4 4 4 4 4 1 3 101 3 1 1 1 1 4 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 0 "Philosophy" 17 1 101 1 0 1 1 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 0 1 0 4 1 2 1 4 4 4 1 2 1 4 3 101 101 6 5 5 4 2 4 4 4 3 5 5 5 5 5 3 4 4 3 3 4 4 4 4 4 4 4 4 0 19 1 4 2 3 4 4 3 3 0 2 1)

  ,(Ans 6 3 4 3 4 4 4 4 4 4 4 3 3 4 3 1 3 4 3 3 2 2 2 2 4 0 0 0 1 0 4 0 0 7 1 0 0 0 0 0 "Business Administration" 12 1 101 1 0 0 0 0 0 0 1 1 2 2 1 1 2 3 0 0 0 0 0 0 0 3 3 3 3 3 3 101 101 101 101 101 101 101 101 3 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 3 4 4 3 1 19 1 4 3 2 2 2 2 2 0 0 1)

  ,(Ans 6 6 4 1 1 4 4 4 1 2 2 2 2 3 1 1 3 1 3 2 2 1 1 1 1 1 0 0 0 0 4 1 1 2 0 0 1 0 0 0 "Art" 14 1 2 1 1 1 0 1 0 0 1 6 2 1 2 1 3 2 0 0 0 0 0 0 0 4 3 4 2 2 4 4 2 4 2 2 4 4 20 5 5 5 5 5 5 5 5 5 5 5 5 3 3 3 5 5 5 2 4 4 3 4 3 3 3 3 1 19 2 3 3 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 1 3 3 3 4 3 3 3 2 2 3 2 3 3 1 3 1 3 4 1 3 2 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "" 12 1 101 1 0 1 0 0 0 0 1 1 101 101 101 101 3 1 0 0 0 0 1 0 0 3 3 3 3 3 3 3 3 1 2 2 3 20 60 6 5 5 5 3 4 3 5 5 5 3 5 3 3 3 3 3 3 2 4 4 2 3 3 3 2 3 1 19 1 3 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 3 4 4 4 3 3 4 4 4 4 4 3 4 4 4 3 4 4 4 3 3 1 1 0 0 0 4 1 1 0 0 0 1 1 0 0 "Theater Arts" 13 1 99 1 0 1 1 1 0 1 1 1 3 101 101 101 101 3 0 0 0 0 1 0 0 1 1 1 2 2 2 101 101 101 101 101 101 101 101 6 5 5 5 3 5 5 5 4 5 5 5 5 5 4 3 3 2 5 3 4 5 4 5 4 5 5 1 19 1 4 5 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 4 2 2 3 4 3 3 4 3 1 4 2 2 1 1 1 2 1 1 1 1 2 1 1 0 0 0 0 4 1 0 7 0 0 1 0 1 0 "Environmental Studies" 17 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 1 2 1 1 0 0 0 0 0 3 2 3 2 4 4 2 2 3 2 3 3 0 2 6 2 3 2 4 5 5 3 2 2 5 2 2 2 2 4 4 4 2 2 3 4 2 4 4 3 4 1 19 1 4 4 3 4 4 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 3 3 1 3 4 2 4 3 1 1 3 1 3 2 2 1 1 3 2 1 0 0 0 1 5 1 1 99 0 0 0 0 0 1 "Computer Science" 15 1 101 1 0 1 1 1 0 1 1 3 3 1 1 2 1 3 1 0 0 0 1 0 0 4 4 4 3 3 3 4 2 3 2 3 2 101 101 6 5 5 5 3 5 5 5 5 5 5 5 4 4 4 3 3 4 4 4 4 2 3 4 4 3 3 0 19 1 4 2 3 4 3 2 3 1 1 1)

  ,(Ans 6 2 4 1 4 4 4 4 2 3 3 3 3 4 4 4 3 2 3 3 3 2 2 3 4 1 0 0 0 0 3 1 1 2 0 0 0 0 1 0 "Psychology" 18 1 99 1 0 1 0 0 0 0 1 6 3 1 2 1 3 1 1 0 0 0 1 0 0 3 3 4 4 3 2 3 2 3 2 2 1 101 79 6 5 5 5 5 5 3 4 5 4 4 3 2 2 3 4 2 2 3 1 2 2 3 4 4 3 3 1 19 2 3 4 3 4 4 3 4 2 2 2)

  ,(Ans 6 4 4 3 3 4 4 4 4 4 4 4 3 4 1 2 4 3 4 3 3 4 4 4 4 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Health Studies" 15 1 101 1 0 0 1 0 0 0 1 6 1 1 2 1 3 1 1 1 0 0 0 0 0 3 3 2 3 1 1 3 3 1 1 1 1 10 37 6 3 4 3 4 4 3 3 3 3 4 5 3 3 3 3 2 3 3 2 3 2 4 2 2 2 1 1 19 1 1 4 3 4 4 2 1 3 0 0)

  ,(Ans 3 5 3 1 4 4 4 3 3 3 3 3 2 2 2 2 2 1 2 1 2 4 1 3 3 0 0 1 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 12 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 2 2 0 1 0 0 0 0 0 3 3 3 2 3 2 2 3 3 2 2 2 1 3 6 5 5 5 2 4 5 3 3 2 3 4 3 3 3 3 3 3 2 3 2 2 2 2 3 1 3 0 19 1 1 1 3 4 4 3 3 0 0 3)

  ,(Ans 6 5 4 3 3 3 4 4 2 4 3 4 4 4 3 1 1 2 2 2 2 1 2 3 3 1 0 0 0 0 4 0 1 0 0 1 1 0 0 0 "Business Administration" 16 1 99 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 1 3 3 2 2 1 2 3 3 2 1 1 2 101 101 6 5 5 5 3 5 5 5 5 5 5 5 4 3 5 4 4 5 4 3 4 2 5 3 4 3 3 1 19 2 3 3 3 4 4 3 3 3 2 3)

  ,(Ans 3 5 3 1 1 4 3 1 4 2 4 2 2 3 3 1 1 1 3 1 2 1 2 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Music" 12 1 2 1 0 1 0 1 0 0 1 1 3 1 2 1 2 3 1 0 0 0 0 0 1 4 3 4 3 2 3 3 3 4 2 1 3 101 101 6 0 5 2 5 5 2 5 3 4 3 4 3 3 2 4 4 2 4 5 4 2 3 4 4 3 3 0 19 1 3 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 3 3 3 4 3 3 3 4 1 1 2 1 3 3 2 3 1 2 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Mechanical Engineering" 18 1 101 1 0 1 1 0 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 3 2 2 4 3 2 2 2 2 1 101 101 6 5 5 3 2 4 3 5 2 3 1 1 3 3 3 3 4 4 2 3 3 3 3 3 4 3 3 1 19 1 2 4 3 4 2 2 3 3 0 3)

  ,(Ans 6 6 4 2 4 3 3 1 2 3 3 3 4 4 3 1 3 3 3 3 3 2 1 2 2 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "Business Administration" 12 1 2 0 0 1 1 0 0 0 1 3 3 1 1 3 1 3 0 0 0 0 1 0 0 4 3 3 2 2 3 3 2 3 2 2 3 101 101 6 5 4 5 2 4 4 4 2 3 4 5 3 3 2 4 4 3 4 3 3 2 3 3 4 3 3 0 19 1 2 3 2 2 2 2 2 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 3 1 3 4 4 4 4 4 1 2 1 3 1 3 1 1 4 4 1 0 0 0 0 4 0 1 0 1 1 1 0 0 0 "International Studies" 14 1 99 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 1 0 1 4 4 4 4 3 4 4 3 4 4 2 4 101 101 6 5 5 5 4 5 3 5 5 5 5 5 4 4 5 2 2 3 4 4 5 3 5 5 5 5 5 1 19 2 2 3 3 4 4 3 3 1 0 1)

  ,(Ans 3 3 2 1 1 4 4 2 1 1 2 1 2 4 1 1 2 4 3 2 3 1 1 3 1 0 0 0 1 0 4 1 1 0 0 0 0 0 0 1 "Political Science" 12 1 99 1 0 0 0 0 0 0 1 3 3 1 2 1 1 3 0 0 0 0 1 0 0 4 3 4 3 3 101 101 101 101 101 101 3 101 101 6 5 5 5 2 5 0 3 4 5 5 4 3 4 2 3 3 2 3 3 4 5 3 4 4 3 4 1 19 1 4 4 3 3 4 3 3 1 2 1)

  ,(Ans 3 3 3 1 4 4 4 3 2 4 2 4 4 1 4 3 2 1 3 1 1 1 1 1 1 1 0 0 0 0 2 0 0 0 0 0 0 0 0 1 "Did not answer" 14 1 101 1 0 1 1 1 0 0 1 2 3 1 2 3 3 1 1 0 0 0 0 0 0 4 3 4 2 4 4 4 1 4 2 2 3 10 40 5 4 5 1 5 5 3 5 4 5 5 0 2 5 2 5 5 2 5 5 5 5 2 5 5 5 5 1 19 1 5 5 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 3 1 2 2 3 1 3 2 4 2 1 1 2 2 1 3 2 1 1 1 1 1 1 1 0 0 0 0 3 0 1 0 0 1 1 0 0 0 "Biology" 12 101 101 1 0 0 0 0 0 0 1 1 1 3 1 1 1 1 0 0 0 0 0 1 0 3 3 3 3 3 3 101 101 101 101 101 101 18 20 6 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 0 19 1 5 4 3 3 3 3 3 0 0 0)

  ,(Ans 3 3 3 1 4 4 4 4 4 4 4 3 3 3 4 4 3 1 3 1 2 2 1 4 3 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Economics" 15 5 101 1 0 1 0 1 0 0 1 1 3 2 101 2 2 2 0 0 0 0 0 0 0 4 3 3 3 3 3 2 2 2 2 2 2 2 10 6 5 4 3 101 4 4 4 101 4 101 2 4 2 2 4 4 2 2 2 4 2 2 4 4 4 4 0 19 1 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 5 3 1 4 3 4 1 3 4 2 4 4 1 4 1 1 1 1 1 1 1 1 3 1 1 0 0 0 0 4 1 1 1 0 0 1 1 0 0 "Undecided" 12 5 2 1 0 1 0 0 0 0 1 2 101 2 101 101 3 99 1 0 0 1 0 0 0 3 4 3 1 2 3 3 4 4 1 1 3 18 60 6 2 3 5 0 4 0 5 4 5 1 5 3 3 4 3 3 3 3 3 3 3 5 4 4 3 2 0 19 2 1 3 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 4 4 4 4 2 2 3 4 1 4 3 2 4 3 1 3 3 3 3 1 4 3 0 1 0 0 0 4 1 1 0 0 0 1 0 0 0 "Architecture" 12 5 2 1 0 1 1 0 0 0 1 3 2 1 1 1 3 2 0 0 0 0 0 0 0 4 3 3 3 3 4 3 3 3 3 3 3 13 45 6 1 2 3 5 2 3 5 0 1 0 3 2 2 2 5 5 4 5 1 2 2 3 3 4 3 3 0 19 1 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 3 4 3 4 4 4 4 4 3 3 3 3 2 2 3 2 2 2 2 2 2 2 0 0 0 1 1 4 1 1 0 0 1 0 0 0 1 "Business Administration" 14 5 2 1 0 0 0 0 0 0 1 1 2 2 101 101 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 5 15 6 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 5 4 4 4 4 4 4 4 4 1 19 1 2 3 3 4 2 3 3 3 2 3)

  ,(Ans 6 4 1 3 2 4 4 4 4 3 3 4 4 4 2 2 2 2 2 2 2 1 1 2 3 0 1 0 0 0 4 1 1 7 0 0 0 0 0 1 "Biology" 101 4 2 1 0 0 1 0 0 0 1 6 1 2 1 1 3 1 0 0 0 0 1 0 0 4 4 3 3 4 4 101 101 101 101 101 101 15 39 1 2 5 4 5 5 4 5 5 5 5 5 2 3 2 4 4 4 4 5 5 5 5 4 5 5 5 1 19 2 1 4 3 4 2 3 3 3 0 0)

  ,(Ans 6 6 3 2 3 2 1 1 4 3 2 4 1 3 1 4 1 1 1 1 2 3 3 4 3 1 0 0 0 0 4 1 1 4 0 0 1 1 0 0 "Theater Arts" 13 4 99 1 0 1 0 1 0 0 1 3 1 2 3 1 2 1 1 1 0 0 0 0 0 2 2 2 3 3 1 4 4 4 4 4 2 16 45 6 5 4 2 1 5 2 5 5 5 3 5 2 3 3 3 4 3 3 101 4 3 3 5 5 5 5 1 19 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 4 4 4 4 1 4 2 4 4 4 3 3 4 4 0 0 0 1 0 4 1 1 7 0 0 1 0 0 0 "Health Studies" 12 3 1 1 0 1 0 0 0 0 1 3 2 1 1 1 3 2 1 0 0 0 0 0 0 3 4 3 2 3 3 3 4 2 2 3 3 2 15 5 5 5 5 2 3 3 1 2 3 2 5 2 3 2 4 4 2 1 2 3 1 3 4 3 3 3 0 19 1 2 2 3 3 2 3 3 0 0 3)

  ,(Ans 6 6 4 3 4 4 4 4 4 4 4 3 3 3 3 4 101 101 101 101 101 101 101 101 101 1 0 0 0 0 4 1 1 0 0 0 1 0 1 0 "Chemistry" 19 2 2 1 0 0 0 0 0 0 1 3 101 101 101 101 3 2 0 0 0 0 0 0 0 4 3 3 3 3 4 3 101 101 101 101 101 101 45 6 2 5 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 19 1 2 1 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 3 4 4 4 2 3 3 3 4 3 4 3 4 3 2 3 3 2 2 3 2 4 1 0 0 0 0 2 1 1 7 0 1 0 0 0 0 "Theater Arts" 12 2 2 1 0 1 0 0 0 0 1 2 1 1 3 1 2 1 0 0 0 0 0 0 0 4 2 3 1 1 4 3 2 2 1 1 4 8 10 6 5 5 5 1 5 5 5 5 4 4 4 3 3 3 4 4 3 4 4 3 3 3 3 3 3 4 0 19 1 99 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 1 4 4 3 1 4 4 4 4 4 2 3 4 2 1 3 1 4 1 1 3 4 0 1 0 0 0 4 1 1 7 0 0 0 0 0 1 "Art" 13 2 2 1 0 1 0 0 0 0 1 1 1 1 1 1 3 1 1 0 0 0 0 0 0 4 4 3 2 4 3 101 101 101 101 101 101 20 50 6 4 5 3 5 5 4 4 3 4 5 5 2 2 2 4 4 3 2 2 2 2 2 3 4 4 4 1 19 2 4 4 3 4 2 2 3 1 1 1)

  ,(Ans 6 6 4 3 4 4 3 4 3 4 4 3 4 4 3 3 3 3 4 4 4 4 3 1 3 1 0 0 0 0 3 0 1 0 1 0 0 0 0 0 "Undecided" 13 2 2 1 0 0 0 0 0 0 2 3 101 101 101 101 3 1 0 1 0 0 0 0 0 3 4 4 3 4 3 2 4 4 3 4 3 25 90 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 19 2 2 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 3 4 3 2 2 3 3 3 2 3 4 3 1 1 2 2 1 1 2 2 3 2 1 1 0 0 0 0 2 0 0 0 0 0 1 0 1 0 "Economics" 17 1 101 0 0 1 1 1 0 0 1 2 3 2 1 1 2 3 1 0 0 0 0 1 1 4 3 1 3 4 4 2 4 1 3 2 2 101 101 6 2 5 3 2 3 5 3 2 5 3 0 3 3 2 3 3 5 3 4 3 2 2 3 4 3 2 1 19 1 5 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 3 4 3 4 3 4 4 4 1 1 3 1 1 3 4 3 3 3 3 3 3 3 1 0 0 0 0 3 1 1 0 0 1 0 0 0 0 "Health Studies" 15 1 99 1 0 0 0 0 0 0 1 3 3 2 2 1 2 3 1 0 0 0 0 0 1 4 4 3 3 3 3 3 4 2 2 2 2 101 101 6 3 3 1 1 2 1 1 1 2 2 1 1 1 1 5 5 1 5 3 2 3 2 4 5 4 4 1 19 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 5 3 4 1 4 4 4 101 1 3 4 1 3 3 4 1 3 2 3 2 3 2 3 101 2 1 0 1 0 0 2 1 1 0 1 0 1 0 0 0 "Business Administration" 12 101 3 1 0 1 1 0 0 0 1 1 2 101 101 101 3 3 1 0 0 0 0 0 0 4 4 4 101 2 1 4 4 4 101 2 1 101 101 6 3 2 3 5 1 0 4 0 0 1 1 1 1 1 1 1 1 2 2 3 3 3 4 3 5 5 0 18 2 4 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 4 4 4 4 4 2 4 1 4 3 3 4 4 4 1 1 1 3 2 1 1 1 1 1 0 0 1 0 0 5 1 1 101 0 0 0 0 0 0 "Computer Engineering" 18 6 3 1 1 1 1 1 0 0 1 1 3 1 1 2 3 3 1 0 0 0 0 0 1 4 3 2 3 4 4 4 3 2 3 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 5 5 5 5 4 5 3 4 5 5 4 4 1 18 1 2 5 3 3 3 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 3 4 2 3 3 1 1 4 1 4 3 1 4 4 3 1 1 1 3 1 0 0 0 0 3 1 1 4 0 0 1 0 0 0 "Psychology" 13 6 2 1 0 1 1 0 0 0 1 6 1 3 1 1 2 1 0 0 0 0 0 0 0 4 4 3 4 3 4 3 3 2 1 2 4 18 30 6 5 5 5 5 5 5 5 5 5 5 5 4 5 2 4 4 4 2 3 3 4 4 3 101 3 3 1 18 2 2 1 3 4 3 3 2 3 0 3)

  ,(Ans 6 5 4 1 4 4 3 4 4 3 4 4 2 3 3 2 3 1 3 2 2 2 3 3 3 1 0 0 0 0 3 1 1 7 0 0 1 1 0 0 "" 12 6 2 1 0 1 1 1 0 0 1 3 1 3 1 1 2 1 1 0 0 0 0 0 0 4 2 3 2 3 4 4 2 3 2 3 4 20 45 6 5 5 5 2 5 3 4 5 5 1 5 4 4 4 4 4 3 3 4 5 2 4 2 5 1 1 0 18 1 5 5 2 2 2 3 2 0 0 0)

  ,(Ans 5 3 1 1 4 4 4 4 2 4 2 2 3 3 1 3 1 3 4 1 3 2 2 2 4 1 0 0 0 0 5 0 1 3 0 0 0 0 0 0 "Business Administration" 15 6 2 1 1 1 1 1 0 0 1 3 1 1 3 1 2 1 0 1 0 0 1 0 0 3 4 4 2 3 2 4 3 3 1 4 1 10 25 6 5 5 5 5 5 4 5 5 5 5 5 4 5 5 5 5 3 3 4 4 5 4 5 5 5 5 1 18 1 99 99 1 3 1 3 3 0 0 0)

  ,(Ans 6 5 4 2 4 4 3 3 4 2 4 1 3 4 2 2 3 2 3 1 4 1 1 3 3 1 0 0 0 0 2 0 0 7 0 0 0 0 0 0 "International Studies" 13 6 101 1 0 1 1 0 0 0 1 6 1 1 1 1 3 2 0 1 0 0 0 1 0 4 4 3 1 3 4 3 3 3 2 3 3 101 25 6 5 5 5 5 5 3 5 5 5 5 3 2 4 3 5 4 1 5 3 4 5 3 4 5 5 5 1 18 2 1 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 6 4 3 4 4 4 1 1 4 4 3 4 4 1 3 3 1 3 2 2 1 3 3 4 1 0 1 0 0 4 1 1 7 0 0 0 0 0 1 "Business Administration" 14 6 2 0 1 1 1 1 0 0 1 3 2 2 2 2 2 2 1 0 0 0 0 0 0 4 3 2 2 4 4 3 1 1 1 2 3 8 45 6 5 5 5 5 5 5 5 5 5 5 5 3 2 2 4 4 4 4 4 4 3 3 3 4 99 4 1 18 1 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 4 2 2 3 4 4 4 3 3 3 1 4 4 4 4 4 4 1 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "International Studies" 15 6 2 1 0 1 0 1 0 0 1 1 3 3 1 1 3 2 1 0 1 0 0 1 0 4 2 2 1 2 1 4 2 2 1 2 1 19 66 6 5 5 5 5 5 2 5 5 5 5 5 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 1 18 2 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 2 4 1 1 4 4 4 4 4 4 4 4 4 4 1 2 1 4 1 1 3 3 4 4 1 0 0 0 0 1 1 0 7 0 0 1 0 0 0 "Undecided" 13 6 2 1 0 1 1 0 0 0 1 3 3 1 1 2 1 3 1 1 1 0 1 1 1 3 3 3 4 3 1 4 3 1 4 1 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 5 5 5 4 5 5 5 99 99 99 99 1 18 2 2 2 3 4 4 2 3 3 3 3)

  ,(Ans 6 2 4 4 4 3 4 4 3 4 3 3 4 3 4 3 4 4 4 3 3 1 2 3 4 1 0 0 0 0 2 1 1 4 1 0 1 0 0 0 "Did not answer" 14 6 101 1 0 1 0 0 0 0 101 1 3 1 1 2 2 3 0 0 0 0 1 0 0 4 4 4 4 4 3 2 3 2 2 3 2 101 101 5 1 1 2 0 5 2 3 0 0 1 2 1 1 1 1 3 3 2 1 1 1 1 4 4 4 3 1 18 1 3 2 3 4 2 2 3 1 0 0)

  ,(Ans 6 6 3 1 4 3 3 1 3 3 1 3 4 4 4 4 1 1 1 1 1 1 2 1 1 1 0 0 0 0 4 1 1 2 0 0 1 0 0 1 "Administration of Justice" 13 6 2 0 0 1 0 1 0 0 1 1 101 2 2 101 2 2 0 0 0 0 0 0 0 3 2 2 1 1 3 3 2 2 1 1 3 13 60 6 5 5 4 0 3 2 2 2 4 0 5 2 3 1 2 3 3 3 3 2 1 3 4 4 2 2 0 18 1 3 2 3 3 2 2 2 3 2 3)

  ,(Ans 3 5 3 3 4 4 4 4 2 4 4 2 3 4 3 1 2 1 4 4 4 2 3 3 3 1 0 0 1 0 4 1 1 0 1 0 0 0 0 1 "Business Administration" 13 6 1 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 0 1 4 4 4 4 3 3 3 2 2 3 2 2 101 101 6 2 1 0 1 0 2 3 3 0 3 1 3 2 2 1 1 3 2 1 1 1 3 4 3 4 4 0 18 1 2 99 3 3 4 3 3 0 0 0)

  ,(Ans 6 3 3 1 3 2 2 3 2 3 2 1 3 4 1 2 3 1 3 1 1 3 2 3 2 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Business Administration" 15 6 2 1 1 1 0 1 0 0 1 6 1 3 2 1 1 1 1 0 0 0 0 0 0 3 3 3 4 4 3 3 2 2 3 4 1 20 45 6 5 5 5 5 5 5 5 5 5 3 3 5 2 4 3 3 4 2 4 5 2 3 3 5 3 4 1 18 2 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 4 3 4 3 4 3 4 3 1 3 1 3 4 2 4 1 2 1 1 0 0 1 0 3 0 1 1 0 0 1 0 0 0 "English" 13 6 2 1 0 0 0 0 0 0 1 1 3 1 1 1 2 2 0 0 0 0 0 0 0 4 3 4 3 4 4 4 2 2 2 4 4 0 10 6 3 3 5 5 3 2 5 4 5 5 5 3 4 5 5 4 3 5 3 4 5 5 3 3 2 2 0 18 2 5 5 3 3 1 3 4 1 2 1)

  ,(Ans 6 6 3 2 3 3 3 2 3 2 3 4 2 4 3 2 2 1 3 2 2 2 4 1 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Administration of Justice" 13 6 2 1 0 0 0 0 0 0 1 2 2 1 2 3 3 3 0 0 0 0 1 0 0 3 2 3 3 3 2 3 4 3 2 2 3 101 101 99 5 5 5 5 5 4 2 5 5 5 5 4 4 4 5 5 4 4 5 4 5 4 5 4 5 5 1 18 1 5 5 3 3 2 3 3 1 0 1)

  ,(Ans 3 6 3 1 3 4 4 1 2 3 3 4 4 3 2 4 4 1 3 2 2 3 2 2 4 1 1 0 0 0 4 1 1 4 0 0 1 0 0 0 "Undecided" 13 5 2 1 0 1 1 1 0 1 1 3 1 1 1 2 3 1 0 0 0 0 0 0 0 4 3 4 3 4 3 3 2 4 1 2 2 2 30 6 5 3 5 2 4 5 4 4 5 2 4 4 4 5 3 3 3 3 4 5 3 4 3 4 2 2 0 18 1 4 2 3 4 2 3 3 3 2 3)

  ,(Ans 6 3 4 2 3 4 3 4 2 4 2 4 2 3 1 2 2 1 3 1 3 1 3 3 4 1 0 1 0 0 3 0 1 0 0 0 0 0 0 1 "Chemistry" 14 5 3 1 0 1 0 1 0 0 2 6 101 101 2 101 3 2 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 2 4 3 101 35 6 5 5 5 5 4 2 5 3 5 5 5 4 4 4 3 3 2 5 3 5 5 5 4 5 4 5 1 18 2 2 1 4 3 2 3 4 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 4 2 4 4 1 2 4 3 2 4 3 4 4 3 2 4 3 2 1 1 1 0 0 5 1 1 99 0 0 0 0 0 1 "Business Administration" 14 5 2 1 0 1 0 0 0 0 1 6 3 2 2 1 3 1 1 1 0 1 0 0 0 4 4 4 4 3 4 4 3 4 3 3 4 35 90 6 5 4 3 5 5 4 4 4 4 4 5 2 2 2 4 4 2 3 4 3 3 3 4 4 3 4 1 18 2 2 1 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 2 1 3 4 2 4 4 3 4 4 2 4 2 3 1 3 3 2 0 1 0 0 0 2 0 0 0 0 0 1 0 0 0 "Architecture" 13 5 2 1 0 1 1 1 0 0 1 1 1 3 2 2 2 2 0 1 0 0 0 0 0 4 3 3 2 3 3 3 2 2 1 3 2 10 30 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 5 4 5 3 4 1 18 1 2 4 3 4 2 3 3 3 2 3)

  ,(Ans 3 5 3 1 4 4 4 3 4 3 4 4 3 2 4 4 3 1 3 2 4 4 2 4 4 1 0 0 0 0 2 0 1 7 0 1 0 0 0 0 "Undecided" 12 5 2 1 1 0 0 0 0 0 1 6 2 101 101 101 3 1 1 0 0 0 0 0 0 1 1 1 1 1 1 3 4 3 2 2 3 25 75 6 1 3 5 5 3 2 2 1 3 1 5 2 3 3 3 3 3 3 2 3 3 3 4 4 3 3 1 18 2 99 99 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 1 1 2 4 4 3 3 2 4 2 1 4 1 1 3 1 4 1 1 0 0 0 0 3 1 1 7 1 1 1 1 0 0 "English" 14 5 2 1 1 1 1 0 0 0 1 2 1 1 1 1 3 1 1 0 0 0 0 0 0 4 4 4 2 4 4 3 2 3 1 3 4 40 120 6 5 5 5 5 5 5 5 3 5 3 5 5 5 5 3 3 4 3 2 5 4 5 3 3 3 3 0 18 2 2 2 2 4 3 3 2 3 0 3)

  ,(Ans 6 2 4 2 2 4 4 4 2 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 1 1 1 7 0 0 0 0 0 1 "Undecided" 15 5 2 1 0 1 0 0 0 0 1 6 1 1 1 1 3 1 0 0 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 20 90 6 2 2 2 2 2 2 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 1 18 2 1 1 3 3 3 3 3 1 0 1)

  ,(Ans 4 4 1 1 4 4 4 4 2 4 4 4 2 2 4 3 3 4 4 2 3 3 2 4 2 1 0 0 0 0 3 1 0 7 0 0 0 0 1 0 "International Studies" 18 5 2 1 0 1 0 1 0 0 1 1 1 3 1 1 1 1 0 0 0 0 1 0 0 4 3 3 3 2 2 3 2 3 3 1 2 25 30 6 5 5 5 5 5 3 5 3 5 4 2 5 5 3 3 3 3 5 3 5 3 5 3 3 3 3 1 18 2 4 4 3 3 3 3 3 1 0 1)

  ,(Ans 6 2 4 3 4 4 4 4 2 3 4 4 4 4 1 3 3 1 4 4 4 3 4 4 4 1 0 0 0 0 4 0 1 0 0 0 0 0 0 0 "Political Science" 13 5 3 1 0 1 1 1 0 1 1 3 1 2 3 1 2 1 1 1 0 0 0 0 0 4 3 2 2 3 3 4 3 2 2 3 3 16 20 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 5 1 18 2 3 3 3 4 3 3 3 3 3 3)

  ,(Ans 6 3 4 2 4 4 3 3 1 4 1 4 3 4 1 1 1 1 1 1 1 1 4 1 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "History" 13 5 2 1 0 1 0 0 0 0 1 6 2 1 1 1 3 2 0 0 0 0 0 0 0 4 3 4 3 4 4 3 2 2 1 2 4 16 60 6 5 5 5 5 5 5 4 2 3 4 5 4 5 3 2 3 2 3 2 2 3 5 5 5 5 5 1 18 2 2 1 1 3 1 3 2 1 2 1)

  ,(Ans 6 4 4 4 4 4 4 4 1 3 4 4 4 4 1 1 1 1 1 1 4 1 1 4 4 1 0 0 0 0 1 1 0 7 0 0 1 0 1 0 "Biology" 12 5 2 0 0 1 1 0 0 0 1 3 3 101 101 101 2 2 1 0 0 0 0 0 0 4 4 4 4 4 4 3 3 2 1 1 4 0 2 6 4 5 4 5 5 5 5 0 5 5 5 2 5 2 5 5 4 4 1 5 5 5 5 5 5 5 1 18 2 2 1 4 3 2 3 2 3 2 3)

  ,(Ans 6 6 3 4 4 3 4 3 3 4 3 4 1 3 4 4 2 1 3 1 2 3 2 1 1 1 1 0 1 0 3 1 1 7 0 0 1 0 0 0 "Architecture" 16 5 2 1 0 1 0 0 0 0 1 1 3 1 1 1 3 1 0 1 0 0 0 0 0 3 4 4 2 3 3 2 3 3 2 3 3 40 50 6 5 5 3 1 5 5 3 2 2 2 5 2 3 3 3 4 101 2 2 3 3 3 3 3 3 3 1 18 1 1 1 3 3 3 3 3 3 0 3)

  ,(Ans 6 4 1 3 3 3 3 3 3 3 3 3 3 101 3 3 3 3 3 3 3 3 3 3 3 1 1 0 1 0 2 1 1 2 0 0 0 0 0 0 "Did not answer" 12 5 101 0 0 0 0 0 1 0 101 3 2 2 2 2 2 1 1 1 0 1 0 1 1 3 3 3 3 3 3 101 101 101 101 101 101 30 60 6 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 5 5 5 5 1 18 2 99 1 2 2 2 2 101 0 0 0)

  ,(Ans 6 6 3 1 3 4 3 1 3 3 3 3 4 2 3 1 2 1 2 2 2 2 1 2 1 1 0 0 0 0 5 1 1 0 0 0 1 0 0 0 "Film" 13 5 2 1 0 1 1 0 0 0 1 3 3 1 1 2 2 2 0 0 0 0 0 0 0 3 2 3 3 3 3 2 1 2 1 2 1 0 5 6 2 4 3 1 2 1 5 2 1 2 1 3 2 3 4 4 3 4 3 3 4 4 4 4 3 3 1 18 2 4 5 3 3 2 3 3 3 2 3)

  ,(Ans 6 6 3 3 4 2 1 4 3 3 4 4 4 4 1 3 3 1 2 2 2 1 1 1 1 1 0 0 0 0 4 1 1 2 0 0 1 0 0 0 "Business Administration" 14 5 101 1 0 0 1 1 0 0 1 1 1 2 2 1 3 1 0 1 0 0 1 0 0 3 4 4 3 2 2 3 4 4 3 2 2 25 40 6 3 3 101 1 5 3 3 3 3 3 3 2 2 5 5 5 3 1 2 3 3 4 3 3 3 3 1 18 1 5 4 3 4 4 2 3 1 0 1)

  ,(Ans 6 3 4 3 3 2 4 2 4 4 3 2 3 3 2 4 4 1 4 2 2 2 3 1 1 1 0 0 0 0 2 1 0 7 1 0 0 0 0 0 "Elementary Education" 13 5 2 1 0 1 0 1 0 0 3 3 1 2 2 1 3 2 0 1 0 0 0 0 0 4 3 3 3 3 4 4 3 3 3 3 4 40 120 6 5 4 5 4 3 3 3 3 3 5 5 2 4 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 2 2 2 2 3 3 3 3 0 0 0)

  ,(Ans 6 4 4 1 4 4 4 4 2 4 3 3 4 3 4 4 4 2 4 2 3 1 1 3 2 1 0 0 0 0 5 0 1 7 0 0 0 0 0 1 "Psychology" 13 5 2 1 0 0 0 0 0 0 1 1 3 1 1 1 1 1 0 1 1 1 0 0 0 4 3 4 3 2 4 101 101 101 101 101 101 1 10 6 5 5 5 5 3 5 5 4 4 3 5 4 4 5 5 5 3 4 2 3 4 5 5 5 4 5 1 18 2 1 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 4 3 4 4 4 1 1 4 4 4 4 4 2 2 2 2 2 3 4 1 1 3 2 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Business Administration" 15 5 2 1 0 0 0 0 0 0 1 1 1 2 3 1 1 1 1 0 0 0 0 0 0 3 3 3 3 3 3 2 3 2 3 3 2 60 85 5 0 4 4 3 3 2 1 1 2 3 4 2 2 1 3 3 2 1 1 1 3 3 4 4 4 4 1 18 1 3 3 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 4 2 3 3 3 1 2 2 2 2 4 1 1 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "English" 8 5 2 1 1 1 1 1 0 0 1 2 2 1 2 1 3 2 0 1 0 0 0 0 0 3 2 3 1 3 3 3 2 3 1 3 3 3 15 4 5 5 5 2 3 1 2 5 5 1 3 5 5 4 4 4 4 5 3 4 4 4 3 4 2 4 1 18 2 4 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 3 1 4 4 4 3 3 2 1 2 1 3 1 1 1 1 1 1 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 15 5 2 1 1 1 1 1 0 0 1 1 101 101 101 101 3 2 0 0 0 0 0 0 0 3 4 3 2 2 2 3 4 4 1 2 1 10 60 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 3 4 4 3 3 3 3 4 3 2 2 1 18 1 3 4 3 4 3 2 1 0 0 3)

  ,(Ans 5 3 2 3 2 3 4 4 3 3 4 3 2 2 3 3 2 2 2 2 2 2 2 3 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "History" 12 5 2 1 1 1 0 1 0 0 1 1 2 2 2 2 1 1 0 0 0 0 0 0 0 2 2 2 2 2 3 2 2 2 2 2 3 11 20 6 5 5 4 3 5 4 5 2 3 3 2 3 3 3 3 3 3 3 3 3 3 3 3 4 3 99 0 18 1 5 4 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 1 4 4 4 3 4 3 3 4 3 1 3 3 2 1 1 2 1 1 0 0 0 0 3 1 1 2 0 0 1 0 0 0 "Accounting" 14 5 2 1 0 1 1 1 0 0 1 1 1 2 2 2 3 1 0 0 0 0 0 0 0 4 3 2 3 4 3 4 4 2 3 4 3 17 25 6 4 5 5 3 5 4 5 4 3 5 5 3 2 3 3 4 3 1 3 3 3 4 5 3 3 4 99 18 1 3 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 4 3 4 4 4 3 2 101 4 4 4 4 3 3 1 3 2 3 1 3 1 1 4 4 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Did not answer" 15 5 3 1 0 1 0 0 0 0 1 6 3 1 1 2 2 3 1 1 1 0 0 0 1 4 4 4 3 4 4 4 3 4 3 4 4 101 101 6 0 5 5 5 5 4 5 5 5 4 3 2 4 4 5 5 5 5 3 4 5 4 4 5 5 5 1 18 2 2 2 3 4 3 3 3 1 1 1)

  ,(Ans 6 6 3 3 4 4 4 3 3 4 3 3 3 3 3 2 3 2 3 3 3 3 1 3 4 0 0 1 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 17 5 2 1 1 1 1 1 0 0 1 6 3 2 2 1 3 1 1 0 0 0 0 0 0 101 101 101 101 101 101 4 3 4 3 3 4 5 20 6 4 5 3 3 3 4 4 3 4 5 5 3 3 2 3 3 3 3 3 3 3 3 4 3 3 3 1 18 2 3 101 3 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 1 4 4 4 2 2 4 4 4 3 4 4 3 3 2 4 4 3 2 1 4 4 1 1 0 0 0 3 1 1 7 0 0 0 0 0 1 "Art" 13 5 99 1 0 1 1 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 3 4 4 3 4 3 4 3 3 2 4 1 101 101 6 5 5 2 5 5 4 2 1 4 0 2 1 3 2 3 3 2 2 1 3 1 3 3 3 3 3 1 18 2 3 3 3 3 2 2 3 1 0 1)

  ,(Ans 6 6 3 3 4 4 4 1 1 3 4 2 3 3 4 101 2 1 3 2 2 1 2 2 1 1 0 0 0 0 3 1 0 0 0 0 1 1 0 0 "European Studies" 12 5 2 1 0 1 0 1 0 0 1 2 3 2 3 1 3 2 0 0 0 0 0 0 0 4 2 3 2 3 3 2 2 3 1 1 1 10 60 6 5 5 3 5 5 2 5 0 4 0 4 1 1 2 3 3 1 3 1 3 2 4 3 4 2 2 1 18 2 2 3 3 4 3 3 3 0 2 1)

  ,(Ans 6 3 4 3 4 4 4 4 1 4 4 4 4 4 4 4 2 1 4 2 3 1 1 1 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Film" 15 5 99 1 0 1 1 1 0 0 1 2 1 1 1 1 3 1 0 0 0 0 0 0 0 3 3 3 3 4 3 1 1 2 1 3 3 10 30 6 5 5 5 5 5 5 5 5 5 5 5 3 2 1 3 4 3 2 3 3 3 3 4 4 3 3 0 18 1 1 99 3 3 4 2 4 0 0 3)

  ,(Ans 6 4 3 1 4 3 3 4 1 2 2 1 1 3 2 4 3 1 3 1 2 1 1 3 1 1 0 0 0 0 2 0 1 0 0 1 0 0 0 0 "Chemistry" 15 5 2 0 1 1 0 0 0 0 1 6 1 1 2 1 3 2 1 0 0 0 0 1 0 4 3 4 4 3 3 4 3 4 3 3 3 4 15 6 5 4 4 5 5 5 5 5 5 5 5 2 2 3 5 5 3 3 2 2 3 4 3 4 2 4 1 18 2 5 99 3 4 2 2 3 1 2 1)

  ,(Ans 6 5 3 3 4 4 4 3 3 3 4 3 3 3 3 4 3 2 3 4 4 4 3 4 2 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Undecided" 15 5 2 1 0 1 1 1 0 0 1 1 2 2 2 3 3 1 0 1 0 0 0 0 0 4 2 3 3 4 4 4 3 4 2 4 4 40 47 6 2 5 5 5 5 5 5 5 5 4 5 3 2 2 5 4 4 4 3 3 4 3 4 4 2 4 1 18 2 3 2 101 101 101 101 101 3 0 3)

  ,(Ans 5 3 3 2 4 4 4 2 2 2 4 3 4 3 3 4 4 1 4 3 3 3 2 4 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Undecided" 14 5 2 1 0 1 1 1 0 0 1 6 2 2 2 1 3 1 1 0 0 0 0 0 0 4 3 4 3 3 2 4 2 101 2 2 2 11 23 6 5 5 5 5 4 5 5 3 5 2 2 4 4 5 4 3 4 4 3 4 3 5 3 4 2 3 1 18 1 5 5 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 3 3 3 3 3 2 2 2 3 3 2 1 1 2 1 3 1 3 2 3 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Foreign Languages" 13 5 2 1 0 1 1 1 0 0 1 1 3 101 101 3 3 2 0 1 0 0 0 0 0 3 4 2 2 3 3 4 4 2 2 2 3 4 45 5 3 5 5 5 3 2 3 0 4 5 2 3 4 3 4 4 2 2 1 4 4 4 5 5 4 5 0 18 2 4 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 3 3 4 3 3 2 4 1 3 3 1 3 2 4 2 1 3 1 1 0 0 0 0 2 0 1 0 0 0 1 0 0 0 "Did not answer" 16 5 3 1 0 1 0 0 0 0 1 2 101 1 2 101 3 1 0 1 0 0 0 0 0 4 4 4 4 2 2 4 4 4 4 2 2 15 40 6 5 5 5 2 5 2 5 0 2 1 5 1 4 3 3 3 3 2 2 3 4 4 4 4 3 4 1 18 2 2 3 3 3 3 2 2 3 0 3)

  ,(Ans 3 6 3 1 4 4 4 3 3 4 2 4 4 4 4 1 3 1 3 3 3 1 1 4 3 0 1 0 0 0 4 1 1 4 1 1 1 0 0 0 "Business Administration" 13 5 2 1 0 1 0 0 0 0 1 3 101 101 101 101 3 2 0 0 0 0 0 0 0 2 2 2 2 4 1 2 2 3 1 3 1 20 40 6 5 5 5 5 5 5 5 3 5 5 5 3 2 3 4 4 2 1 1 2 3 3 3 5 2 2 1 18 2 2 1 3 4 2 2 4 3 2 3)

  ,(Ans 6 6 4 2 2 2 3 1 1 1 1 3 4 3 3 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 1 1 4 0 0 0 0 0 1 "Computer Science" 16 5 2 1 0 0 0 0 0 0 1 2 3 101 101 2 101 3 0 0 0 0 0 0 0 3 3 3 3 3 3 2 2 2 2 2 2 101 101 6 5 5 5 0 1 5 2 5 5 0 5 4 5 5 5 5 4 3 5 5 1 5 1 4 1 1 0 18 1 2 3 3 3 4 3 3 0 0 0)

  ,(Ans 6 5 4 1 1 4 4 2 2 1 2 3 4 2 2 2 2 1 3 2 1 4 3 2 1 0 0 0 0 1 3 1 1 7 0 0 0 0 0 0 "Foreign Languages" 12 5 2 1 0 0 0 0 0 0 1 1 3 1 1 1 3 2 0 0 0 0 0 0 0 4 3 3 4 3 2 101 101 101 101 101 101 101 45 5 2 3 3 0 0 3 0 2 5 0 0 2 4 2 4 4 3 3 3 4 2 3 3 4 2 4 1 18 2 4 3 3 4 2 3 3 3 0 3)

  ,(Ans 3 5 3 1 4 4 4 4 2 4 4 4 3 3 2 2 3 3 3 3 4 4 3 3 4 1 0 0 0 0 2 1 1 0 1 0 1 0 1 0 "Foreign Languages" 16 5 2 1 0 1 0 0 0 0 1 2 3 1 1 2 3 1 1 1 0 0 0 1 0 3 4 2 2 2 3 101 101 101 101 101 101 10 60 6 5 4 4 3 3 3 5 2 3 2 3 1 1 2 3 3 4 3 2 2 2 3 5 4 4 4 1 18 2 2 2 3 1 1 2 3 3 0 0)

  ,(Ans 6 6 4 4 3 3 4 4 4 4 3 4 3 3 3 4 4 1 4 3 3 4 3 2 4 1 0 0 0 0 4 1 1 7 0 0 0 1 0 1 "Biology" 15 5 2 1 0 1 1 1 0 0 1 6 1 3 2 1 1 1 0 0 0 0 0 0 0 4 4 3 3 3 3 3 3 2 1 1 2 20 35 5 5 5 2 0 4 1 3 4 3 0 1 3 4 1 4 3 1 1 4 3 1 3 4 4 3 3 1 18 2 2 2 3 3 3 3 3 1 2 1)

  ,(Ans 6 3 4 4 4 101 4 4 4 4 4 4 4 3 1 4 3 1 3 1 1 2 3 1 1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 "Business Administration" 14 5 2 1 0 1 1 1 0 0 1 1 1 1 3 1 1 1 0 0 0 0 0 0 0 4 4 4 3 3 4 4 3 2 1 2 3 35 60 6 1 3 5 5 5 5 5 2 5 5 5 3 4 4 4 4 4 5 3 4 4 4 5 5 5 5 1 18 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 4 4 4 3 4 2 4 4 4 4 4 1 4 3 1 4 1 3 2 3 4 4 0 1 0 0 0 3 1 1 0 0 0 0 0 0 1 "Business Administration" 13 5 2 1 0 0 1 1 0 0 3 2 1 2 2 1 3 1 0 0 0 0 0 0 0 4 3 3 3 3 4 4 3 3 3 3 4 5 30 6 5 5 5 3 5 5 5 5 5 0 5 1 1 1 3 3 1 1 3 1 1 2 3 3 3 3 0 18 2 2 1 3 3 3 3 3 0 0 0)

  ,(Ans 6 5 4 1 4 4 4 4 2 4 4 3 3 3 3 4 4 1 3 1 3 1 1 2 3 1 0 0 0 0 3 0 1 4 0 0 1 0 0 0 "Business Administration" 13 5 2 1 0 1 1 1 0 0 1 3 1 2 2 1 3 1 0 0 0 0 0 0 0 4 4 3 1 4 3 3 101 2 1 4 3 15 45 6 5 5 5 5 5 4 5 3 5 3 5 4 3 4 3 3 4 3 4 3 2 4 4 4 3 4 1 18 2 2 3 3 3 3 3 2 1 0 1)

  ,(Ans 5 3 2 1 3 2 2 3 3 3 2 3 4 2 3 4 3 2 2 2 2 3 3 2 3 1 0 0 0 0 3 1 1 0 0 1 1 0 1 0 "Undecided" 15 5 2 1 1 1 0 0 0 0 1 3 1 2 1 1 3 1 1 0 0 0 0 0 0 2 2 1 2 2 2 4 3 4 2 2 2 12 40 5 5 5 3 3 3 2 2 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 0 18 1 1 1 3 2 1 1 1 1 2 1)

  ,(Ans 6 1 4 3 4 4 4 4 2 4 4 4 4 2 4 4 4 101 2 2 1 4 4 2 1 0 0 0 0 1 5 1 1 0 1 1 1 0 0 0 "Mechanical Engineering" 13 5 2 1 0 1 0 0 0 0 1 1 3 101 101 101 3 1 1 0 0 1 0 0 0 4 3 4 3 3 4 101 101 101 101 101 101 7 50 6 4 4 2 0 4 4 3 2 4 2 4 3 5 5 5 5 5 5 5 5 5 5 3 3 3 3 0 18 1 2 5 101 101 101 101 101 101 101 101)

  ,(Ans 5 3 4 2 4 4 3 4 2 3 1 3 1 3 1 4 2 2 2 3 3 2 3 2 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Electrical Engineering" 15 5 2 1 0 1 1 1 0 0 1 1 101 3 101 101 101 2 0 0 0 0 1 1 0 3 4 3 3 3 3 4 3 2 3 3 3 20 30 6 2 4 2 5 5 5 3 2 4 101 5 3 4 4 4 4 5 3 3 4 3 4 4 4 3 4 1 18 1 3 2 3 3 2 3 3 3 0 3)

  ,(Ans 3 5 4 3 4 4 3 2 2 4 4 3 3 4 3 2 1 1 1 1 2 1 1 4 3 1 0 0 0 0 2 1 1 4 1 0 0 0 0 0 "Art" 5 5 2 1 0 1 1 0 0 0 1 1 1 1 2 1 3 2 0 0 0 0 0 0 0 4 4 4 3 4 4 2 2 2 1 3 3 4 45 6 5 5 4 5 4 2 5 5 5 4 5 4 4 3 5 5 3 4 3 3 4 5 4 4 2 3 1 18 2 2 2 3 3 4 3 2 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 3 4 4 4 4 3 3 2 4 4 2 3 2 2 3 3 2 3 1 0 0 0 0 3 1 1 7 0 0 0 0 1 0 "Administration of Justice" 15 5 2 1 0 1 1 1 0 0 1 1 3 1 2 2 3 1 1 0 0 1 1 0 0 4 3 3 2 2 2 3 2 2 2 2 2 15 45 6 5 5 5 5 5 5 5 5 5 5 5 4 3 3 4 4 5 5 5 4 5 5 4 4 4 4 1 18 1 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 3 5 3 3 2 4 4 1 4 3 3 1 4 3 3 4 4 1 3 2 3 1 3 3 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Undecided" 16 5 2 1 0 0 0 0 0 0 1 3 1 1 2 1 3 1 0 0 0 0 0 0 0 3 2 3 2 2 2 4 1 3 1 1 1 5 45 6 2 4 1 5 5 0 4 0 4 3 2 1 2 1 3 3 1 3 1 3 3 3 3 4 3 4 1 18 2 3 1 3 3 3 3 3 0 0 0)

  ,(Ans 5 5 3 4 4 4 4 4 2 4 4 4 1 4 4 1 4 1 4 4 4 1 1 4 4 1 0 0 0 0 4 101 1 0 0 0 0 0 0 1 "Did not answer" 101 4 1 1 0 0 0 0 0 0 101 3 3 1 1 1 1 3 1 0 0 1 0 0 1 4 4 4 4 4 4 3 4 4 4 3 4 101 101 6 5 5 5 5 5 5 5 2 5 4 5 3 5 2 5 4 4 2 1 5 3 5 4 5 5 5 1 18 2 1 4 3 4 3 3 3 0 0 0)

  ,(Ans 6 3 3 4 4 2 3 3 2 4 3 4 2 4 4 2 3 4 3 2 2 1 2 3 4 1 0 0 0 0 3 1 1 7 0 1 1 0 0 0 "Mechanical Engineering" 18 4 99 0 0 1 0 0 0 1 1 6 3 1 1 1 2 3 1 0 0 0 1 1 0 3 2 2 3 3 3 4 2 3 2 3 4 101 101 6 5 5 5 5 5 5 4 5 5 5 5 3 4 4 5 5 5 3 4 5 3 4 3 4 99 3 1 18 1 4 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 6 4 101 3 3 3 1 2 3 3 4 3 3 3 4 3 2 3 3 3 2 1 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Did not answer" 16 4 101 1 0 0 0 0 0 0 1 101 1 2 3 1 2 101 0 0 0 0 0 0 0 3 3 2 2 3 3 2 3 2 2 3 2 8 30 99 3 1 4 0 0 2 1 4 5 2 4 3 3 3 2 2 3 5 4 4 3 4 3 4 99 99 0 18 1 4 4 3 4 4 2 3 3 3 3)

  ,(Ans 6 6 2 4 4 4 4 3 1 4 3 4 3 3 3 1 3 2 3 3 3 1 1 3 4 1 0 0 1 0 3 1 1 7 1 0 1 0 0 0 "Chemistry" 14 4 99 0 0 0 0 0 0 0 1 3 3 2 1 2 2 3 1 1 0 0 0 1 1 3 4 3 4 3 2 4 4 2 4 2 1 101 101 6 5 5 4 3 5 4 5 5 5 5 5 3 4 5 3 5 4 4 3 5 3 5 4 5 5 5 1 18 2 3 3 3 3 101 3 3 0 0 0)

  ,(Ans 6 2 4 1 2 4 4 4 1 4 1 3 2 3 1 4 3 1 3 1 2 1 1 1 4 1 0 0 0 0 3 0 1 4 0 0 0 0 0 1 "Biology" 12 4 2 1 1 1 1 0 0 0 1 6 1 3 1 1 1 2 0 0 0 0 0 0 0 4 4 3 2 3 3 3 2 2 1 2 1 20 30 6 5 5 5 5 5 5 4 5 5 3 5 4 4 3 4 3 2 2 3 4 4 5 4 4 3 5 0 18 2 2 2 3 3 2 2 3 3 2 3)

  ,(Ans 6 2 4 4 4 4 4 4 1 4 2 4 4 4 4 4 4 1 4 1 2 4 4 4 4 1 0 0 0 0 4 0 1 0 0 1 0 0 0 0 "Chemistry" 16 4 2 1 0 1 1 1 0 0 3 2 3 2 2 1 3 1 0 0 0 0 0 1 0 4 3 4 2 3 4 4 2 3 2 1 3 20 40 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 5 5 4 4 5 5 5 4 4 1 3 1 18 1 3 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 2 4 1 4 4 4 4 1 3 4 1 4 1 4 3 3 1 3 4 1 1 1 4 4 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Chemistry" 15 4 2 1 0 1 1 1 0 1 3 6 3 1 2 1 3 1 0 0 0 0 0 0 0 4 4 4 2 4 4 4 4 4 2 4 4 25 60 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 4 4 3 3 3 4 4 5 3 3 4 4 0 18 2 5 2 3 4 4 3 3 3 0 3)

  ,(Ans 3 5 2 1 4 4 3 2 1 4 3 3 3 3 4 1 4 2 3 2 3 1 3 3 4 1 0 0 0 0 4 1 0 7 0 0 1 0 0 0 "Undecided" 12 4 3 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 0 1 0 0 0 0 0 3 3 3 2 4 4 2 2 1 1 3 4 101 101 99 2 5 4 3 3 4 4 1 4 5 4 1 4 4 4 3 4 2 1 3 4 3 4 4 3 3 1 18 2 4 3 2 3 3 3 3 1 1 1)

  ,(Ans 6 6 3 3 4 4 4 2 1 4 4 4 1 4 1 4 2 1 2 1 1 1 1 1 1 1 0 0 0 0 4 1 1 7 0 0 1 1 0 0 "Film" 13 4 2 1 0 1 1 1 0 0 3 2 2 3 1 1 1 1 1 0 0 0 0 0 0 4 4 4 2 3 4 4 3 2 2 2 4 10 25 6 5 5 5 5 5 2 4 3 4 4 5 5 4 2 5 5 3 2 3 5 3 5 2 2 1 1 1 18 2 3 2 3 4 3 2 2 3 2 3)

  ,(Ans 6 6 4 2 4 4 4 3 4 4 2 4 3 4 2 4 4 1 4 3 4 4 1 3 4 1 1 0 0 0 4 1 1 7 1 0 0 0 0 0 "Accounting" 14 4 2 1 0 0 0 0 0 0 1 3 3 101 101 101 3 1 1 0 0 0 0 0 0 4 4 3 101 4 3 4 3 3 101 4 3 18 100 5 5 5 5 5 5 5 5 5 5 3 5 2 2 3 4 4 3 2 3 2 2 3 3 3 4 3 1 18 2 1 1 3 3 3 3 3 0 0 3)

  ,(Ans 6 5 1 1 4 4 4 4 3 4 2 4 3 2 2 1 2 1 2 2 2 1 3 2 4 1 0 0 0 0 2 1 0 7 0 0 1 0 0 0 "Environmental Studies" 13 4 99 0 0 0 1 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 1 101 101 101 101 101 3 2 4 1 1 4 101 101 6 2 5 1 5 5 5 5 5 5 5 5 2 3 1 4 4 3 1 3 4 4 4 4 4 4 4 1 18 2 4 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 4 1 3 4 4 4 1 2 4 3 3 4 1 1 2 1 2 1 4 1 1 3 1 0 1 0 0 0 5 1 1 7 0 0 0 0 0 0 "International Studies" 14 4 2 1 1 1 0 0 0 0 1 6 3 1 1 2 2 3 1 0 0 0 0 1 0 4 4 4 3 3 4 4 3 4 3 3 4 101 101 4 5 5 5 3 2 1 5 3 5 4 5 5 5 3 4 4 5 5 3 5 5 5 4 5 3 4 1 18 2 4 3 3 4 2 2 2 3 0 3)

  ,(Ans 6 3 4 4 4 4 4 4 1 3 3 4 4 4 4 1 4 1 4 4 4 2 3 4 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "International Studies" 101 4 2 1 0 0 0 0 0 0 1 6 3 1 1 1 3 3 0 0 0 0 0 1 1 1 1 1 1 1 1 101 101 101 101 101 101 101 101 6 5 5 5 2 2 5 4 3 4 1 5 2 1 1 3 3 2 2 3 2 3 3 4 4 3 4 1 18 2 3 1 2 4 2 3 3 3 3 3)

  ,(Ans 3 4 3 1 3 3 3 3 1 3 3 3 1 2 2 4 3 1 3 3 2 1 1 1 1 1 0 0 0 0 4 0 1 0 0 0 1 1 0 0 "Undecided" 15 4 2 1 1 0 1 1 0 0 1 1 2 2 2 1 3 1 0 1 0 0 0 0 0 101 101 101 101 101 101 3 3 2 3 2 1 50 40 6 2 4 2 4 2 2 3 2 3 1 5 1 3 2 3 3 3 1 2 3 3 4 4 3 4 4 1 18 2 3 5 3 3 2 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 3 4 1 3 2 3 3 3 1 3 1 3 3 3 1 1 3 2 0 0 0 0 1 5 1 1 0 0 0 0 0 0 1 "Undecided" 15 4 2 1 0 1 1 0 0 0 1 6 3 1 1 1 1 3 1 1 0 0 1 1 1 4 4 4 4 4 4 4 4 4 3 3 3 101 101 5 2 5 5 5 3 3 5 3 4 5 5 3 4 3 5 5 4 5 4 4 5 4 5 5 5 5 1 18 2 4 3 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 3 4 4 2 3 2 4 4 2 4 1 4 3 1 1 1 2 2 2 1 1 1 3 1 0 0 0 0 4 1 1 7 1 1 0 0 0 0 "Architecture" 12 4 2 1 0 0 0 0 0 0 1 101 3 101 101 101 101 3 0 0 0 0 0 1 0 1 1 2 2 2 2 1 2 2 2 2 2 101 101 6 5 3 4 0 3 0 5 0 3 0 2 2 3 3 3 3 1 3 2 3 2 3 3 2 2 2 0 18 1 2 99 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 1 2 4 4 4 4 1 4 4 3 3 4 3 4 4 2 4 3 3 4 1 3 4 1 0 0 0 0 4 1 1 7 1 0 0 0 0 0 "Art" 17 4 2 1 0 1 0 0 0 0 1 6 3 3 1 1 3 1 0 1 0 0 0 0 0 4 3 2 4 4 3 3 2 2 3 4 3 15 45 6 2 3 3 5 1 1 4 1 3 0 5 3 3 2 4 5 4 2 2 3 2 4 5 4 3 5 1 18 2 2 4 4 3 3 3 3 3 2 3)

  ,(Ans 6 2 4 4 4 4 4 4 2 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 0 0 0 3 1 1 0 0 1 0 0 0 0 "Undecided" 15 4 2 1 0 0 1 0 0 0 1 2 3 3 1 1 1 1 1 1 0 0 0 0 0 4 4 4 3 3 4 4 3 3 2 2 2 28 25 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 3 3 4 3 4 3 5 4 5 1 18 1 1 1 3 3 2 3 2 0 0 0)

  ,(Ans 6 3 4 2 4 4 4 4 1 3 4 4 4 3 4 4 2 1 2 1 1 3 3 4 4 1 0 0 0 0 2 1 1 7 0 0 0 0 0 0 "Art" 13 4 2 0 0 1 1 1 0 0 1 6 2 2 2 2 3 1 1 1 0 0 0 1 0 4 4 4 4 4 4 4 3 3 3 4 2 7 45 6 4 4 2 2 5 1 2 3 5 5 5 3 4 2 5 4 1 2 3 3 5 4 4 5 4 5 1 18 2 2 2 3 4 3 3 3 1 0 1)

  ,(Ans 6 3 3 1 2 4 3 2 3 3 2 4 3 3 3 3 3 3 3 1 3 3 3 3 4 0 0 0 0 1 2 0 0 7 0 0 1 1 0 0 "Social Work" 12 4 2 1 1 1 1 1 0 0 1 2 2 1 2 2 3 1 0 1 0 0 0 0 0 4 3 4 3 2 4 4 3 4 2 2 4 10 15 6 5 5 5 5 5 5 5 5 5 5 5 2 3 2 4 5 2 3 2 2 4 2 5 5 5 5 1 18 2 5 2 3 3 3 3 3 1 1 1)

  ,(Ans 6 2 4 3 1 3 4 4 3 3 3 4 3 4 2 4 4 3 4 2 3 3 3 2 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Biology" 14 4 2 1 0 1 1 1 0 0 1 3 3 2 1 2 3 1 0 0 0 0 1 0 0 3 4 3 3 3 3 3 4 3 2 101 3 10 25 6 5 5 4 3 5 5 5 5 5 5 5 4 4 3 3 4 4 3 3 3 4 4 3 3 3 4 1 18 1 3 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 2 4 2 4 4 4 4 4 4 4 4 4 2 1 4 4 1 3 3 1 1 4 4 1 1 0 0 0 1 2 1 1 0 1 0 1 0 0 0 "Biology" 16 4 2 1 1 1 1 1 0 0 1 1 3 1 2 1 3 1 1 0 0 0 0 0 0 4 4 3 2 3 4 4 4 3 1 2 4 10 30 6 5 5 5 2 5 5 3 0 5 5 4 2 1 1 4 4 3 2 1 4 3 5 4 5 3 2 1 18 2 4 1 3 4 3 3 2 1 2 1)

  ,(Ans 3 6 3 4 4 4 4 1 3 3 4 4 2 1 1 4 1 1 1 1 1 1 1 3 4 1 0 0 0 0 1 0 1 99 0 0 0 0 0 0 "Psychology" 12 4 2 1 1 1 1 1 0 0 1 6 2 2 101 1 3 1 1 1 0 0 0 1 0 3 101 3 101 101 101 101 3 101 3 3 3 23 80 6 2 5 5 5 5 5 5 0 5 0 5 1 2 2 5 5 2 3 1 1 1 1 4 3 2 4 1 18 2 99 99 3 3 3 3 3 0 0 0)

  ,(Ans 5 2 3 3 4 4 3 4 3 4 4 4 4 4 3 4 3 2 3 3 4 3 1 3 4 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "Science" 14 4 2 1 0 1 1 1 0 0 1 6 2 1 1 1 3 1 1 0 0 0 0 0 0 4 3 2 1 3 2 1 1 1 1 1 101 5 50 6 5 5 5 5 5 5 5 5 5 5 5 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 2 2 2 3 3 4 4 4 0 0 0)

  ,(Ans 6 6 4 2 2 4 4 1 3 3 3 3 4 1 1 1 3 1 3 3 3 2 3 4 2 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Undecided" 12 4 2 1 1 1 1 1 0 0 1 2 3 1 1 1 2 3 0 0 0 0 0 0 1 4 3 4 2 2 4 3 3 3 2 2 3 101 101 6 5 5 5 5 5 5 4 3 5 5 5 3 5 5 4 4 3 1 2 4 3 3 4 4 5 5 1 18 2 4 99 3 2 3 3 3 3 2 3)

  ,(Ans 6 4 3 4 4 3 4 4 3 4 3 4 4 3 3 4 2 3 3 3 4 3 4 3 4 1 1 0 0 0 4 1 0 7 0 0 1 1 0 0 "Computer Engineering" 17 4 3 1 0 1 1 1 0 1 1 6 2 2 2 1 3 1 1 1 0 0 0 1 0 4 4 3 3 3 3 4 3 3 3 3 2 30 30 6 3 5 3 5 4 5 5 3 5 3 4 3 4 3 5 5 4 2 2 4 3 4 3 4 3 3 1 18 1 1 1 3 3 2 3 3 1 0 1)

  ,(Ans 6 2 4 3 4 4 4 4 4 4 4 3 2 4 3 1 4 2 4 4 4 2 3 4 4 1 0 0 0 0 2 0 1 101 1 1 1 0 0 0 "Chemistry" 14 4 3 1 0 1 1 1 0 0 1 6 1 1 3 1 2 1 1 0 0 0 1 0 0 4 3 3 2 4 4 4 3 3 2 4 4 100 60 6 5 5 4 3 5 5 5 5 5 4 5 3 4 2 3 4 2 2 2 1 1 3 3 5 3 4 0 18 2 5 2 3 3 3 3 3 0 0 0)

  ,(Ans 6 6 4 1 4 3 3 2 3 3 4 3 2 4 3 3 3 3 3 3 3 2 2 3 1 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Did not answer" 12 4 2 1 0 1 1 1 0 0 1 1 3 1 2 1 3 1 0 0 0 0 0 0 0 4 3 3 3 4 3 3 2 2 2 2 2 45 65 6 1 5 2 2 3 0 5 0 2 2 2 2 3 3 4 5 2 3 2 3 3 3 4 4 4 4 1 18 1 3 3 3 3 2 3 3 1 2 1)

  ,(Ans 6 6 4 2 4 4 4 2 1 2 1 2 3 3 2 3 2 1 3 2 1 3 1 3 3 1 0 0 1 0 4 0 1 0 1 1 1 0 0 0 "Foreign Languages" 12 4 2 1 1 1 1 1 0 0 1 3 3 101 2 101 3 1 0 0 0 0 0 0 0 4 3 4 3 4 3 3 3 3 3 2 2 10 60 6 5 4 5 3 5 5 5 4 5 2 2 3 3 3 4 4 4 3 3 3 2 3 4 4 99 99 0 18 2 1 1 3 3 3 3 3 0 0 1)

  ,(Ans 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 1 0 0 0 3 0 1 0 0 0 1 0 0 0 "Undecided" 13 4 3 1 1 1 0 0 0 0 3 6 1 1 1 1 3 1 1 1 0 1 1 0 0 4 4 4 3 3 3 4 3 2 2 2 1 101 30 6 5 5 5 5 5 3 5 5 5 3 5 2 2 2 2 3 2 1 1 1 1 2 4 4 4 4 1 18 2 1 3 3 3 2 3 2 1 2 101)

  ,(Ans 6 3 4 1 4 4 3 4 3 4 4 4 4 4 2 1 4 2 3 1 2 1 1 3 4 1 0 0 0 0 4 1 101 7 0 0 0 0 0 0 "Business Administration" 15 4 2 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 1 1 0 1 1 0 0 3 3 3 2 3 2 4 3 2 2 3 2 101 101 6 5 5 5 5 5 5 5 4 5 5 3 4 4 4 4 4 4 5 3 5 5 5 4 4 4 4 1 18 2 1 1 3 3 3 3 3 1 2 1)

  ,(Ans 6 2 4 3 2 4 4 4 4 4 3 4 4 4 4 4 4 1 4 3 4 3 3 3 3 1 0 0 0 0 3 1 1 0 1 0 1 0 0 0 "Business Administration" 15 4 2 1 0 1 1 1 0 0 1 2 1 1 2 1 3 1 0 0 0 0 0 0 1 1 1 2 2 1 2 101 101 101 101 101 101 10 25 6 2 5 5 0 3 1 3 3 2 5 4 3 4 4 5 5 4 4 2 3 2 3 3 4 2 3 0 18 2 2 2 3 3 2 4 3 0 0 0)

  ,(Ans 6 4 4 1 1 4 4 4 3 2 3 3 4 3 2 1 3 1 3 1 4 1 1 4 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "English" 15 4 99 1 0 1 0 0 0 0 1 3 3 1 1 3 2 3 1 0 1 0 0 0 0 4 4 4 3 3 4 3 3 3 2 3 4 101 101 6 5 5 5 4 5 4 5 5 5 4 5 5 5 4 4 4 4 5 4 5 4 4 3 4 4 4 0 18 2 3 2 3 4 4 3 3 1 1 1)

  ,(Ans 99 3 4 3 4 4 4 3 4 3 4 3 2 4 3 3 4 3 4 4 3 4 1 2 1 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Social Work" 12 4 2 1 0 1 1 0 0 0 1 1 2 2 1 1 3 1 1 1 0 0 0 0 0 4 3 4 2 3 4 4 2 3 1 1 3 6 45 6 5 3 4 2 5 4 5 0 5 1 0 4 5 4 5 5 4 5 2 4 4 3 4 4 3 2 1 18 2 99 4 3 2 3 3 3 1 2 1)

  ,(Ans 6 6 4 3 3 4 4 1 2 3 4 3 3 4 2 1 3 2 4 3 2 3 2 4 2 1 0 0 0 0 4 1 1 0 1 0 1 0 0 0 "Business Administration" 16 4 3 1 0 1 0 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 1 0 1 4 4 4 3 4 3 3 3 4 2 3 2 101 101 6 5 5 5 5 5 2 5 3 5 5 5 2 4 4 3 3 4 2 3 3 4 4 5 4 5 5 1 18 2 99 4 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 4 4 4 3 1 4 4 4 4 4 4 1 4 3 1 3 3 1 3 1 3 4 1 0 0 0 0 5 1 1 7 0 0 0 0 0 0 "Civil Engineering" 13 4 2 1 1 1 1 1 0 0 1 3 2 1 1 3 3 1 1 0 0 0 0 0 0 4 4 4 4 4 4 4 4 3 4 1 3 7 37 6 5 5 5 5 5 5 5 5 5 2 5 4 4 3 5 5 5 4 5 4 4 4 4 4 3 3 0 18 1 4 3 3 4 3 3 3 1 1 1)

  ,(Ans 6 5 4 3 4 3 4 4 4 4 3 3 3 4 3 4 3 1 1 1 1 2 2 1 1 1 0 1 0 0 3 1 1 7 0 0 1 0 0 0 "Computer Engineering" 13 4 2 1 0 1 1 1 0 1 1 1 2 1 1 1 3 1 0 0 0 0 0 1 0 3 3 3 3 3 2 3 3 2 3 3 2 18 65 6 5 5 5 5 4 5 3 5 5 4 2 2 2 3 4 5 4 3 3 3 3 3 3 4 3 4 1 18 1 2 3 3 3 1 3 3 3 2 3)

  ,(Ans 6 3 101 3 4 4 3 4 2 4 4 3 3 4 2 3 3 1 3 2 3 3 2 3 1 1 0 0 0 1 4 1 0 2 0 0 0 0 0 0 "Psychology" 12 4 2 1 0 1 0 0 0 0 1 1 2 1 1 1 3 1 0 1 0 0 0 1 0 4 3 2 2 2 4 101 101 101 101 101 101 25 80 6 5 5 5 5 5 5 5 5 5 5 5 3 4 5 5 4 4 4 3 4 4 4 5 5 5 5 1 18 2 2 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 5 3 1 2 4 3 2 4 3 3 4 4 2 2 3 2 1 2 1 2 4 4 3 2 1 0 0 0 0 3 1 0 7 0 0 0 0 0 1 "Undecided" 12 4 2 1 1 0 1 1 0 0 1 3 3 2 2 1 3 1 0 0 0 0 0 0 0 4 3 3 2 4 3 3 2 2 1 4 3 15 70 6 4 5 3 5 5 5 5 5 5 3 5 3 5 3 5 5 4 3 3 3 3 3 4 5 3 4 1 18 2 3 3 3 3 2 3 3 3 2 3)

  ,(Ans 6 6 4 3 2 3 2 1 2 3 3 4 3 3 3 2 4 2 2 2 3 1 1 2 2 1 0 0 0 0 4 0 1 7 0 0 1 0 0 0 "Accounting" 13 4 2 1 0 1 1 0 0 0 1 3 3 2 1 1 2 3 0 0 0 0 0 0 0 4 4 2 3 2 3 3 2 2 2 2 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 3 4 5 4 5 5 5 4 3 4 3 4 2 2 1 18 1 4 5 3 4 3 2 3 3 3 3)

  ,(Ans 6 4 4 1 4 4 4 4 2 4 3 3 4 3 3 1 3 1 3 1 4 1 1 4 3 1 0 0 0 0 3 0 0 7 0 0 0 0 0 1 "Undecided" 14 4 2 1 0 1 1 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 101 101 101 101 101 101 3 1 3 1 1 4 101 101 6 5 5 5 1 5 1 5 1 3 1 1 3 3 2 5 5 3 3 2 3 4 3 5 5 5 3 1 18 2 4 4 3 4 3 3 3 0 0 1)

  ,(Ans 6 3 4 1 4 4 3 2 2 4 3 3 4 4 4 4 4 3 3 2 4 4 4 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Computer Engineering" 14 4 3 1 1 0 0 0 0 0 1 3 1 1 1 1 3 1 1 0 1 0 1 0 0 4 4 4 4 3 3 4 3 3 3 3 2 8 50 6 0 2 2 5 5 5 4 3 2 5 2 2 3 3 4 4 3 4 2 3 5 3 4 4 4 4 1 18 1 99 1 2 2 3 3 2 1 2 1)

  ,(Ans 3 3 3 2 3 3 4 3 3 4 2 4 4 3 3 3 2 1 3 2 3 101 2 2 3 1 0 0 0 0 3 0 1 0 0 0 1 0 0 0 "English" 14 4 2 1 0 0 0 1 0 0 1 1 2 2 2 1 3 1 0 0 0 0 0 0 0 3 3 2 3 3 3 2 2 1 2 3 2 25 30 6 5 5 3 2 5 2 5 2 1 3 3 3 3 2 3 2 4 3 2 3 3 3 4 3 3 2 0 18 2 1 3 3 3 2 3 3 3 0 3)

  ,(Ans 6 3 3 3 4 4 4 4 4 4 3 2 3 4 4 4 3 2 4 4 4 2 1 4 4 1 0 0 0 0 4 1 1 0 1 0 1 0 0 0 "Biology" 12 4 2 0 0 1 0 0 0 0 3 3 1 1 2 1 3 2 1 1 0 0 0 1 0 4 4 4 4 4 4 4 4 3 3 4 3 25 90 6 4 5 5 5 5 5 5 5 5 5 5 3 3 3 5 5 3 4 3 4 4 3 4 5 5 5 0 18 1 2 2 3 1 3 3 2 3 0 3)

  ,(Ans 6 5 4 1 4 4 3 2 3 4 3 3 3 4 2 4 3 1 3 1 2 2 1 2 3 1 0 0 0 0 4 0 1 4 0 0 0 0 0 0 "Foreign Languages" 12 4 101 1 0 1 1 1 0 0 1 2 1 1 2 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 2 2 2 2 2 2 5 45 6 5 5 5 5 5 4 5 5 5 5 5 4 4 4 4 4 3 4 4 4 4 4 5 4 3 3 1 18 1 5 5 3 4 4 3 3 0 101 3)

  ,(Ans 6 2 4 1 3 4 2 4 2 3 3 3 4 4 3 3 2 1 4 3 3 2 1 4 3 1 0 0 0 0 4 0 1 0 0 0 0 0 0 0 "Psychology" 15 4 101 1 0 1 1 1 0 0 1 2 3 1 2 3 3 3 1 1 1 0 0 0 0 4 3 3 1 4 3 4 3 3 1 4 3 101 101 6 3 5 2 3 5 4 5 5 5 5 5 2 4 3 5 4 4 5 3 3 3 4 4 5 4 3 0 18 2 3 3 3 3 4 3 3 1 2 1)

  ,(Ans 5 3 2 2 4 4 4 4 2 4 4 3 3 3 3 1 4 1 4 4 1 3 1 4 4 1 0 0 0 0 4 1 0 0 1 1 1 0 0 0 "Undecided" 13 4 2 1 0 1 1 1 0 0 1 6 3 1 1 3 1 3 1 0 0 0 0 1 0 4 4 3 4 4 4 4 3 3 3 4 4 101 101 6 5 5 5 4 5 4 5 5 5 5 5 2 3 3 5 5 5 5 3 3 5 4 4 4 3 4 1 18 2 1 2 3 4 3 3 3 1 1 1)

  ,(Ans 6 3 4 3 3 101 101 4 3 4 2 4 3 4 3 4 3 1 1 101 2 3 3 3 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 14 4 2 1 0 1 1 1 0 0 1 6 1 3 1 1 1 1 0 0 0 0 0 0 0 3 2 3 2 3 4 3 2 3 2 3 4 101 15 6 5 5 3 5 101 5 4 3 4 1 4 3 2 3 3 4 3 3 1 3 3 3 3 4 4 4 0 18 2 2 5 3 3 3 3 3 0 0 0)

  ,(Ans 4 4 2 3 4 4 4 4 3 4 3 2 2 4 4 4 4 3 4 3 3 3 3 3 4 1 0 0 0 0 4 0 1 0 1 1 1 0 0 0 "Biology" 12 4 2 1 1 1 1 1 0 0 1 6 1 2 3 1 1 1 1 0 0 0 0 0 0 4 3 4 4 4 4 4 2 3 2 3 3 10 30 6 5 5 5 5 5 5 5 5 5 2 5 3 3 2 4 5 5 3 3 2 2 2 5 5 5 5 1 18 1 4 5 3 3 3 3 3 1 0 1)

  ,(Ans 6 6 3 1 3 3 3 2 2 3 3 3 3 2 2 4 3 1 3 1 1 2 3 4 3 1 0 0 0 0 2 1 1 2 0 0 0 0 1 0 "Art" 12 4 2 1 0 1 0 0 0 0 1 1 2 1 2 1 3 1 0 0 0 0 0 1 0 3 3 3 2 3 2 2 3 2 1 1 3 10 45 6 5 5 3 5 5 5 3 3 3 1 1 3 4 5 5 3 2 2 2 3 2 4 3 4 3 3 0 18 2 3 4 3 101 3 3 3 3 0 3)

  ,(Ans 3 3 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 3 4 3 4 4 4 4 4 1 0 0 0 0 3 1 1 7 0 0 0 0 0 0 "Undecided" 17 4 2 1 0 1 1 0 0 0 1 1 1 2 3 1 2 2 1 1 1 0 0 0 0 3 2 2 1 1 2 3 2 2 1 1 2 15 50 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 5 5 4 5 5 4 5 5 5 5 4 4 1 18 1 5 4 3 4 2 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 4 4 4 4 3 4 4 2 1 3 2 4 2 2 1 1 1 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Administration of Justice" 14 4 2 1 1 1 0 1 0 0 1 2 2 2 2 101 101 3 1 0 0 0 1 0 1 1 101 101 3 2 2 4 3 2 2 3 3 101 101 6 5 5 5 4 5 3 5 3 3 5 5 4 4 5 5 5 3 4 3 3 4 5 5 5 5 5 1 18 1 4 4 3 4 2 3 3 1 2 1)

  ,(Ans 6 3 4 3 4 4 4 3 1 4 4 4 4 4 4 3 4 1 3 4 4 1 1 4 4 1 0 0 0 0 3 1 0 7 0 0 1 0 0 0 "Elementary Education" 9 4 2 1 0 0 0 0 0 0 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 4 2 4 3 3 3 4 2 4 1 4 2 20 60 6 3 5 5 3 5 5 2 4 101 3 5 3 4 3 5 5 3 1 2 4 1 3 5 5 5 5 1 18 2 2 2 3 4 3 3 2 1 0 1)

  ,(Ans 6 6 4 1 4 4 4 1 1 2 2 4 4 4 1 1 3 1 3 2 2 1 1 2 1 0 0 0 0 1 4 1 1 7 1 0 0 0 0 0 "International Studies" 17 4 101 1 0 0 0 0 0 0 1 1 3 2 2 3 1 3 0 0 0 0 0 0 0 4 3 4 2 4 3 3 2 3 1 4 3 101 101 6 5 5 5 4 5 2 2 3 5 4 4 5 5 4 4 3 2 2 4 5 4 5 5 5 5 5 1 18 2 2 3 3 3 3 3 3 1 0 1)

  ,(Ans 3 5 3 3 3 4 4 1 2 3 3 3 3 2 2 1 2 1 2 3 4 3 1 3 1 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Undecided" 101 4 2 1 0 0 0 0 0 0 1 3 3 101 101 101 3 2 0 0 0 0 0 0 0 4 3 4 3 3 3 3 2 3 2 2 3 1 20 6 5 5 5 1 5 3 4 1 5 0 5 2 1 2 3 4 3 2 3 4 3 3 4 5 99 99 1 18 2 2 2 3 3 2 3 2 1 1 1)

  ,(Ans 6 5 4 2 4 4 4 2 1 3 3 3 3 4 3 4 3 2 3 2 3 4 1 4 3 1 0 0 0 0 3 0 1 0 1 0 0 0 0 0 "Undecided" 13 4 2 1 1 1 0 1 0 0 1 6 1 3 1 1 1 1 1 0 0 0 1 0 0 3 101 101 101 101 101 101 2 3 1 2 1 20 30 6 4 5 3 5 5 3 5 4 4 4 5 3 4 2 5 5 3 3 3 5 3 4 3 4 4 4 1 18 2 2 4 3 3 3 3 3 0 0 101)

  ,(Ans 99 6 3 101 4 3 4 3 3 4 4 4 4 4 3 1 3 2 2 2 3 3 2 4 3 1 0 0 0 0 2 1 1 7 0 1 1 0 0 0 "Theater Arts" 13 4 2 1 0 1 0 1 0 1 1 2 3 1 2 3 3 3 1 1 0 0 1 0 0 3 4 3 3 3 3 4 4 3 3 3 3 101 101 6 2 5 3 5 5 3 4 3 3 5 5 2 2 2 4 3 3 2 2 4 4 3 3 3 3 3 0 18 2 4 2 2 4 3 3 2 1 2 1)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 4 4 1 4 1 3 2 1 3 3 4 4 2 1 3 1 0 0 0 0 4 1 0 0 0 0 0 0 0 0 "Architecture" 18 4 2 1 0 1 1 1 0 0 1 6 1 3 1 1 1 1 0 0 0 0 0 0 1 3 3 3 3 3 3 3 3 2 1 3 3 15 25 6 2 5 5 5 5 5 5 5 5 5 2 2 4 3 4 4 4 2 3 4 3 4 3 3 3 3 1 18 2 2 2 3 2 2 3 3 0 0 0)

  ,(Ans 3 6 2 1 3 2 4 1 2 3 4 2 3 2 2 1 1 1 1 3 3 1 1 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 30 4 99 0 0 0 0 0 0 1 1 1 3 1 1 1 2 3 1 1 0 0 0 0 0 3 4 3 3 4 4 3 4 3 3 4 4 101 101 6 0 4 3 0 3 0 4 2 5 5 5 2 2 3 5 5 1 4 1 4 5 2 4 4 2 2 1 18 2 2 5 3 4 4 3 3 3 0 3)

  ,(Ans 6 4 4 4 4 4 4 4 3 3 3 2 3 4 3 2 2 2 3 3 3 3 1 2 4 1 0 0 0 0 5 1 1 7 0 0 0 0 1 0 "Business Administration" 15 4 3 1 0 0 0 0 0 0 2 6 1 3 3 1 2 1 1 1 0 0 1 1 0 3 3 3 3 3 3 3 3 3 3 3 3 101 20 6 1 2 1 1 2 1 1 1 1 1 1 2 3 3 4 4 3 3 2 2 2 2 5 4 4 5 1 18 2 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 4 3 4 4 4 4 4 4 4 4 2 4 3 3 2 3 1 2 1 1 1 1 2 1 1 1 0 0 1 3 0 1 1 0 0 0 1 0 0 "Biology" 30 4 3 1 1 1 1 1 0 0 1 1 2 3 2 3 2 2 1 0 1 0 0 1 0 4 4 1 2 1 3 4 4 1 1 1 1 3 10 5 5 5 5 5 5 5 5 5 5 5 3 5 5 2 4 5 5 4 4 4 4 4 3 5 2 4 1 18 1 4 5 101 101 101 101 101 101 101 101)

  ,(Ans 5 6 3 3 4 4 4 2 4 2 4 4 3 3 1 3 1 1 1 1 1 1 2 2 1 1 0 0 0 0 3 0 0 1 0 0 0 0 0 1 "Architecture" 15 4 2 1 0 1 1 0 0 0 1 3 1 3 1 1 1 1 0 0 0 0 0 0 0 4 3 3 1 3 4 3 2 2 1 2 3 40 45 6 2 5 3 5 2 3 5 4 2 3 1 3 3 3 5 5 4 5 2 3 4 3 5 4 3 5 1 18 1 1 1 3 4 3 3 3 1 1 1)

  ,(Ans 6 6 4 4 4 4 4 1 3 4 4 1 1 1 1 1 1 4 1 1 1 1 1 1 4 1 0 0 0 0 3 0 1 0 1 0 0 0 0 0 "Undecided" 13 4 1 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 2 4 101 1 3 2 2 2 4 2 2 3 101 101 6 5 5 4 0 4 4 3 2 2 3 4 3 2 2 2 4 3 2 2 3 3 4 3 3 99 99 0 18 1 2 1 3 3 4 4 4 0 0 0)

  ,(Ans 6 6 4 3 4 4 4 2 2 4 4 4 3 3 2 2 2 2 2 2 3 2 2 2 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Business Administration" 16 4 2 1 1 1 1 1 0 0 1 2 1 2 2 1 1 1 1 0 0 0 1 0 0 3 3 3 3 3 3 2 3 3 3 3 3 10 20 6 5 5 4 5 3 3 2 101 5 3 4 3 3 3 4 4 2 3 2 3 3 4 3 3 4 4 1 18 1 1 4 2 2 2 3 3 1 0 1)

  ,(Ans 6 3 4 4 4 2 3 4 1 2 1 4 4 4 2 4 4 1 4 4 3 4 1 1 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 15 4 99 1 0 1 1 1 0 0 1 2 1 3 3 1 1 1 0 1 0 0 0 0 0 4 3 2 2 4 3 3 2 1 1 3 3 27 35 6 5 5 5 5 5 5 5 5 5 5 4 4 5 4 5 5 5 5 5 3 5 5 4 3 3 3 1 18 2 2 4 3 4 4 3 3 0 0 1)

  ,(Ans 6 2 3 1 3 4 4 4 3 3 4 4 3 3 3 1 3 1 3 3 4 3 3 3 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Undecided" 15 4 101 1 0 1 1 0 0 0 1 6 3 1 1 2 1 3 0 0 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 101 101 6 3 5 4 4 4 4 4 4 4 4 3 101 101 101 101 101 101 101 101 101 101 101 5 5 5 5 1 18 1 4 3 3 101 101 3 3 1 0 1)

  ,(Ans 6 6 3 2 4 3 3 3 4 4 2 4 2 2 2 3 2 2 2 2 2 1 1 1 1 1 0 0 0 0 2 1 1 0 1 0 1 0 0 0 "Business Administration" 13 4 2 1 1 0 0 0 0 0 1 1 1 3 1 1 1 1 0 0 0 0 0 0 0 4 2 2 1 3 1 4 2 3 1 3 1 25 45 6 2 2 0 1 2 1 2 2 2 0 0 3 4 2 3 4 3 1 2 3 3 4 4 3 2 2 1 18 2 2 2 3 3 2 2 1 3 2 3)

  ,(Ans 5 6 2 1 2 4 4 3 2 2 4 2 4 2 2 1 3 1 3 3 4 3 3 4 1 1 0 0 0 0 4 1 1 4 0 1 0 0 0 0 "Psychology" 14 4 2 1 0 1 0 0 0 1 1 1 3 1 1 1 2 3 1 1 0 0 0 0 0 2 2 3 3 3 3 1 1 2 2 3 3 101 101 6 5 5 5 2 5 4 3 4 4 2 4 3 3 1 5 5 5 2 3 3 3 3 4 4 3 4 1 18 2 99 4 3 4 2 2 3 3 3 3)

  ,(Ans 6 4 4 1 4 4 4 4 2 3 2 4 3 4 3 1 3 2 4 4 3 2 1 2 2 1 0 0 0 0 4 1 1 7 1 0 1 0 0 0 "English" 16 4 2 1 0 1 1 0 0 0 1 6 3 2 101 101 101 3 0 0 0 0 0 0 0 101 3 101 101 3 101 4 101 3 4 101 2 101 101 6 0 5 5 3 4 4 3 2 5 3 2 3 3 2 2 3 3 1 2 3 3 3 4 4 4 4 1 18 2 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 3 2 2 3 4 4 4 4 4 4 3 3 4 2 1 1 1 1 1 1 1 1 1 3 1 1 0 0 0 0 4 1 0 7 0 0 0 0 0 1 "Psychology" 15 4 3 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 1 0 0 0 0 0 0 4 3 2 2 4 3 4 3 2 1 4 3 101 101 6 5 5 5 3 5 5 5 5 5 0 4 4 4 2 5 3 2 3 1 3 4 4 3 4 2 2 1 18 2 4 5 3 3 4 2 4 3 3 3)

  ,(Ans 6 4 4 1 4 2 101 4 1 3 2 4 1 4 2 1 3 1 4 2 2 1 1 3 4 1 1 0 0 0 3 1 1 7 0 0 0 0 0 1 "Chemistry" 14 4 2 1 1 1 1 1 0 0 1 6 1 2 2 1 3 2 1 0 0 0 0 0 0 4 3 4 2 1 3 4 2 3 2 1 3 30 15 6 3 5 5 5 5 5 5 2 4 5 4 3 4 2 5 5 4 3 1 3 4 3 4 3 3 4 1 18 2 1 1 3 4 3 3 3 3 0 3)

  ,(Ans 6 5 4 2 4 4 4 4 2 3 4 3 4 4 2 3 4 1 4 2 3 2 3 3 3 1 1 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 15 4 2 1 0 1 1 1 0 0 1 6 1 3 2 1 2 1 1 1 0 0 0 0 0 4 3 3 3 3 3 4 2 3 3 3 3 13 30 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 1 18 2 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 2 4 1 3 4 4 4 4 4 3 4 4 4 3 4 4 1 4 2 4 4 1 3 4 1 0 0 0 0 4 1 1 7 1 1 0 0 0 0 "Chemistry" 12 4 2 1 0 0 0 1 0 0 1 3 1 2 1 1 3 1 1 1 0 0 0 0 0 4 3 4 1 3 4 4 2 4 1 3 4 25 85 6 5 2 4 5 5 2 3 1 4 5 4 3 2 3 5 5 2 3 1 4 4 3 4 3 4 5 1 18 2 4 4 3 3 3 3 4 0 101 3)

  ,(Ans 6 5 4 1 4 4 4 2 2 3 3 3 4 2 1 3 3 1 2 1 1 1 2 3 4 1 0 0 0 0 2 0 1 4 0 0 0 0 0 1 "Biology" 13 4 2 1 0 1 1 1 0 0 1 6 3 101 101 101 3 1 0 0 0 0 0 0 0 3 2 4 2 4 2 3 2 3 1 3 3 101 70 6 5 5 5 5 5 5 4 4 5 4 4 5 4 5 3 2 3 1 3 5 3 5 4 4 2 3 1 18 2 1 3 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 3 4 3 4 4 4 4 4 4 4 3 3 4 3 2 3 3 3 4 4 2 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Elementary Education" 13 4 2 1 0 0 0 0 0 0 1 2 1 2 3 1 2 1 0 0 0 0 0 0 0 3 2 2 2 2 3 3 2 2 2 2 3 18 40 6 5 5 5 4 5 3 4 4 4 4 4 3 3 2 3 3 3 2 3 3 3 3 4 4 4 4 1 18 2 3 4 3 4 2 3 2 1 1 1)

  ,(Ans 6 3 3 1 4 3 3 3 2 3 2 3 2 4 1 1 3 3 3 1 1 2 2 2 4 1 0 1 1 0 4 1 0 7 0 0 1 0 0 0 "Music" 21 4 2 1 0 0 0 0 0 0 1 3 1 3 1 1 1 1 0 0 0 0 1 0 0 4 3 4 2 3 4 3 2 2 1 2 3 60 60 6 5 5 5 5 5 0 4 4 3 1 5 3 5 3 5 5 1 2 3 4 4 3 3 4 3 3 1 18 2 5 5 3 4 3 2 3 1 1 1)

  ,(Ans 3 4 3 1 4 4 4 4 1 3 2 4 3 1 1 1 1 1 1 1 3 1 1 3 3 1 0 0 0 0 3 1 1 0 0 1 0 0 0 0 "Psychology" 12 4 2 1 0 1 0 0 0 0 1 6 1 1 1 2 3 2 1 0 1 0 0 1 0 4 4 4 4 4 3 4 3 4 2 4 1 20 90 6 5 5 2 5 5 2 5 5 5 5 5 3 3 2 5 5 2 4 3 4 5 3 5 5 5 5 1 18 2 2 2 3 3 2 3 3 3 3 3)

  ,(Ans 6 3 4 3 4 4 4 4 2 3 3 3 3 4 2 1 3 1 3 2 4 3 1 4 2 1 0 0 0 0 5 1 0 7 1 1 1 0 0 0 "International Studies" 13 4 2 1 0 1 0 0 0 0 1 1 3 1 2 2 2 3 1 1 0 0 0 0 0 4 4 4 4 4 3 4 4 4 4 4 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 3 4 5 4 5 5 4 5 5 5 5 5 4 5 1 18 2 4 3 3 4 2 3 3 3 0 3)

  ,(Ans 6 3 4 1 1 4 4 4 1 3 2 3 1 3 2 3 4 1 4 1 2 4 1 2 3 1 0 0 0 0 1 0 1 4 0 0 0 0 0 1 "Business Administration" 12 4 2 1 0 1 0 0 0 0 1 3 101 3 101 101 101 1 0 0 0 0 0 0 0 4 3 3 4 4 3 3 2 3 4 4 3 10 25 6 2 5 4 5 3 3 3 2 2 0 3 2 4 3 5 5 4 3 3 3 3 5 4 4 3 3 1 18 2 1 1 3 3 3 3 3 0 2 1)

  ,(Ans 6 6 3 3 4 4 3 1 3 4 3 2 3 4 3 4 3 1 2 2 2 2 3 2 1 1 0 0 0 0 5 1 1 0 0 1 1 0 0 0 "Computer Science" 5 4 2 1 1 1 1 1 0 0 1 1 1 1 1 1 3 1 0 0 0 0 0 0 0 4 3 2 2 2 2 2 2 1 1 1 1 9 15 6 5 5 5 1 3 5 5 5 5 4 5 3 3 3 3 3 3 3 3 2 2 2 2 2 1 1 1 18 1 2 4 3 3 3 3 2 1 0 1)

  ,(Ans 6 3 3 1 4 4 4 3 2 4 3 3 3 3 3 3 3 2 3 3 2 3 2 3 4 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Biology" 15 4 2 1 0 0 0 0 0 0 1 3 1 3 1 1 1 1 1 0 0 0 0 0 0 4 3 3 1 2 2 101 101 101 101 101 101 12 18 6 5 5 5 4 5 5 5 5 5 5 5 4 4 4 4 4 4 4 101 4 4 4 4 5 3 4 1 18 1 2 3 3 3 3 3 2 3 0 3)

  ,(Ans 6 6 4 3 3 4 4 4 1 4 4 2 3 3 2 1 1 1 1 1 2 1 1 4 1 1 0 0 0 0 3 1 1 7 0 0 0 0 0 1 "Did not answer" 14 4 99 1 0 0 0 0 0 1 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 4 2 3 3 4 2 4 2 3 3 4 2 101 101 6 3 3 3 3 3 0 3 1 1 0 1 4 4 4 3 4 2 5 3 4 3 3 3 3 3 101 99 18 1 4 4 3 4 4 3 3 3 0 0)

  ,(Ans 6 6 4 1 4 3 4 3 1 4 4 4 4 1 1 1 1 1 1 1 2 1 1 4 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Applied Linguistics" 13 4 2 1 0 1 0 0 0 0 1 3 3 101 101 101 3 3 0 0 0 0 1 0 0 4 4 1 1 3 1 4 3 1 1 2 1 101 101 6 5 5 5 3 5 0 5 3 5 5 2 3 3 3 4 4 101 4 2 3 4 4 4 4 4 4 1 18 2 2 5 3 3 3 3 3 3 0 3)

  ,(Ans 3 5 3 2 2 3 3 2 2 2 2 3 3 3 3 1 2 1 2 1 1 2 3 2 2 1 1 0 0 0 3 1 1 99 0 0 0 0 0 0 "English" 13 4 99 1 0 1 1 1 0 0 1 1 3 1 2 1 2 3 1 0 0 0 0 0 0 3 2 3 2 4 101 3 2 3 2 4 3 101 101 6 3 5 5 0 4 2 3 2 5 4 0 2 4 3 3 3 2 4 2 3 4 3 3 3 3 101 0 18 1 5 5 3 101 2 3 101 3 0 3)

  ,(Ans 6 6 4 2 4 4 4 3 4 4 3 2 4 3 4 4 3 1 3 2 3 4 1 2 4 1 0 0 0 0 4 1 0 7 0 1 1 0 0 0 "Art" 15 4 2 0 0 1 0 1 0 0 1 6 1 1 2 1 3 2 1 0 0 0 0 0 0 4 3 4 2 4 2 3 1 2 1 3 3 20 70 6 5 5 5 3 5 4 4 4 5 5 4 3 3 3 4 4 4 2 3 4 2 4 3 4 5 3 0 18 2 1 2 3 2 1 3 2 1 1 1)

  ,(Ans 3 3 2 4 4 3 2 4 2 4 4 4 3 3 4 1 1 1 1 1 1 1 2 2 1 1 0 0 0 0 3 1 0 0 0 0 0 0 0 1 "Did not answer" 15 4 2 1 0 0 1 0 0 0 1 1 3 1 1 2 1 2 0 1 0 0 1 0 0 4 3 3 3 2 4 3 2 2 2 1 3 0 3 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 5 5 5 2 3 4 3 3 3 4 2 2 1 18 1 4 4 3 3 4 3 4 1 1 1)

  ,(Ans 5 1 1 1 4 4 4 4 1 2 2 4 3 4 3 4 3 1 3 4 3 1 1 1 3 1 0 0 0 0 3 1 1 4 0 1 0 0 0 0 "Did not answer" 9 4 2 1 0 1 0 1 0 0 1 3 1 2 2 2 3 1 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 2 3 4 7 50 6 3 5 5 3 2 1 5 0 4 3 5 3 3 3 3 3 3 4 1 3 4 3 3 5 5 5 1 18 1 2 1 4 4 4 4 4 0 0 0)

  ,(Ans 6 6 4 1 3 4 4 3 1 3 4 2 4 1 2 1 2 1 1 1 3 4 1 2 4 1 0 0 1 0 3 1 1 7 0 0 0 0 0 0 "Business Administration" 14 4 2 1 0 1 1 1 0 0 1 6 1 1 2 1 3 2 1 1 0 0 0 0 0 3 3 4 4 3 3 4 3 4 4 2 3 10 45 6 5 5 5 5 5 5 5 3 5 5 5 4 3 5 4 5 4 4 2 4 5 3 4 4 4 4 1 18 1 5 3 3 2 4 2 3 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 3 3 4 3 3 4 4 4 3 2 4 4 4 3 2 2 2 1 0 0 0 0 2 1 0 0 0 0 1 0 0 0 "Business Administration" 13 4 2 1 0 1 1 1 0 0 1 2 3 1 3 1 3 1 1 0 0 0 1 1 0 4 4 3 4 3 4 4 4 3 4 2 4 20 45 6 5 5 5 5 5 5 5 5 5 5 5 2 2 3 5 5 5 5 4 4 5 4 3 4 3 3 1 18 1 5 2 3 3 3 3 3 3 3 3)

  ,(Ans 3 4 4 4 4 2 3 4 1 3 1 3 2 2 2 3 1 1 1 1 2 1 1 1 1 1 0 0 0 0 2 1 1 2 0 0 1 0 0 0 "Psychology" 13 4 101 1 0 0 0 0 0 0 1 6 1 2 3 1 2 1 0 1 0 0 0 0 0 3 3 3 2 3 3 3 3 3 3 3 3 10 10 6 5 5 5 3 5 2 3 0 5 3 5 3 4 4 3 3 2 1 1 4 4 4 4 4 3 3 1 18 2 3 2 3 3 4 4 3 3 2 3)

  ,(Ans 6 5 4 1 3 3 2 2 1 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Art" 14 4 99 1 0 1 1 1 0 1 1 1 3 1 2 2 3 3 0 0 0 0 1 0 0 4 3 3 2 3 4 3 2 2 1 2 2 101 101 6 5 5 3 4 5 5 5 3 5 3 5 3 2 1 4 4 4 4 2 3 3 3 3 4 2 3 1 18 1 4 4 3 3 4 3 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 4 3 1 3 4 1 4 4 4 4 4 1 1 3 4 1 1 4 4 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Communication Studies" 12 4 99 1 0 1 1 0 0 0 1 6 3 1 2 2 2 3 1 0 0 0 0 0 1 1 1 1 101 101 101 3 3 4 3 4 2 101 101 6 4 5 5 2 4 3 4 2 5 5 5 3 3 3 3 2 3 1 2 3 3 3 4 4 2 2 1 18 2 2 2 3 3 4 3 3 3 2 3)

  ,(Ans 6 3 2 3 4 4 3 4 2 4 4 4 4 4 4 4 3 1 4 3 4 3 2 4 4 1 0 0 0 0 2 0 1 7 1 1 1 0 0 0 "Psychology" 13 4 2 1 0 1 0 0 0 0 1 3 2 101 101 101 3 1 1 1 0 0 0 0 0 1 1 1 2 1 1 101 101 101 101 101 101 30 60 6 5 5 5 5 5 5 5 5 5 4 5 2 5 5 5 5 5 5 3 5 2 5 3 4 3 5 1 18 2 2 3 3 2 2 2 2 3 0 3)

  ,(Ans 6 3 3 1 4 3 3 3 2 4 3 4 3 2 3 4 3 1 2 2 2 1 2 3 2 1 0 0 0 0 2 1 1 0 0 1 0 0 0 0 "Biology" 15 4 2 1 0 0 0 0 0 0 1 1 1 2 1 1 3 1 0 0 0 0 0 0 0 4 3 3 3 3 3 4 3 3 3 3 3 25 55 6 3 2 5 5 5 4 5 4 4 3 5 3 2 2 5 5 3 4 2 2 3 3 4 4 3 4 0 18 2 2 4 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 2 2 3 2 4 3 4 1 3 1 3 2 2 1 1 4 2 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Undecided" 16 4 101 1 0 0 0 0 0 0 1 1 3 1 1 2 3 3 1 0 0 0 0 1 0 4 3 3 2 2 4 3 2 2 2 2 4 101 101 6 0 5 5 2 2 2 3 3 5 5 4 2 3 3 3 3 2 3 3 3 3 3 3 4 2 2 0 18 1 5 4 3 4 4 2 3 3 3 3)

  ,(Ans 6 6 3 4 4 2 3 2 4 4 2 4 3 4 3 4 3 1 2 3 3 2 1 2 1 1 0 0 0 0 3 0 1 7 0 0 1 0 0 0 "Computer Science" 17 4 2 1 0 1 0 0 0 0 1 1 3 1 1 1 3 1 1 0 0 0 0 0 0 3 4 2 2 3 4 2 2 3 1 4 3 35 30 6 2 1 1 0 0 1 1 0 1 0 0 1 1 1 1 1 3 1 1 2 1 2 3 4 2 3 0 18 1 4 5 4 3 2 3 2 1 0 1)

  ,(Ans 6 5 4 1 4 3 4 2 2 3 4 4 1 3 2 1 4 2 3 2 2 3 1 3 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 16 4 2 1 0 0 0 0 0 0 1 1 3 101 101 101 2 2 0 0 0 0 0 0 0 4 3 4 2 3 1 4 2 3 1 2 1 0 5 6 3 5 3 3 2 4 2 3 1 4 1 3 4 2 3 3 2 2 3 3 5 5 4 4 3 4 1 18 1 4 2 3 4 2 3 3 1 2 1)

  ,(Ans 6 6 4 1 4 3 3 2 4 3 2 4 4 4 4 4 4 1 3 1 1 1 1 3 4 1 0 0 0 0 3 1 1 99 1 1 1 1 0 0 "Business Administration" 13 4 2 1 0 1 0 0 0 0 1 3 1 1 1 1 3 1 0 0 0 0 0 0 0 101 3 3 3 3 3 3 101 101 101 101 101 15 60 6 5 5 5 5 5 5 5 3 5 5 5 3 3 3 4 4 3 4 3 3 4 4 3 3 3 3 0 18 2 2 2 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 4 0 1 7 0 0 1 0 0 0 "Elementary Education" 13 4 2 1 0 0 0 0 0 0 3 4 2 101 101 101 3 1 1 0 0 0 0 0 0 101 101 101 101 101 101 2 2 2 2 2 2 30 5 6 3 3 3 3 3 4 4 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 5 5 5 5 1 18 2 2 1 3 3 3 3 3 1 0 0)

  ,(Ans 6 5 3 2 4 4 4 3 3 2 4 3 2 4 1 101 2 1 3 1 3 1 1 1 3 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Administration of Justice" 15 4 2 1 0 0 0 0 0 0 1 3 3 1 1 2 2 3 0 0 1 1 0 0 0 3 3 3 2 2 3 3 3 3 1 1 3 101 101 6 0 4 3 3 1 0 1 0 1 0 2 1 2 1 4 4 3 4 1 1 2 2 4 3 4 4 1 18 2 4 1 3 4 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 3 4 2 3 2 4 3 2 4 4 2 1 3 1 3 2 4 1 1 2 2 1 0 0 0 1 4 1 1 7 1 0 1 0 0 0 "Art" 16 3 101 1 0 0 0 0 0 0 1 2 3 1 1 2 3 3 1 1 0 0 0 0 0 4 4 4 3 4 4 3 2 4 101 4 3 101 101 101 5 5 5 4 5 4 5 3 5 5 5 2 3 3 4 5 2 3 2 3 3 4 5 4 5 5 1 18 2 3 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 3 4 4 4 4 4 4 3 4 4 3 2 3 4 2 3 2 2 2 2 3 4 1 0 0 0 0 3 0 1 7 0 0 0 0 1 0 "English" 17 3 2 1 0 1 1 1 0 0 1 2 1 1 2 1 3 1 1 1 0 0 0 0 0 4 4 4 1 4 3 3 3 3 1 4 2 10 60 6 5 5 5 5 5 4 4 5 5 4 4 5 5 4 4 4 4 3 2 4 3 5 3 4 2 4 1 18 2 2 2 2 3 3 3 2 3 0 3)

  ,(Ans 3 6 101 4 4 4 4 4 2 4 4 4 4 4 4 1 4 3 4 4 4 4 4 4 4 0 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Business Administration" 15 3 1 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 0 0 0 0 1 0 0 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 1 18 1 3 3 3 3 4 3 3 3 0 3)

  ,(Ans 3 5 3 2 1 4 4 3 3 4 3 1 4 4 3 1 3 1 3 3 3 2 1 1 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Undecided" 12 3 2 1 0 0 0 0 0 0 1 1 3 1 1 1 1 2 1 1 0 0 0 1 0 4 4 4 4 3 4 4 4 4 3 2 2 101 5 6 5 5 5 5 5 5 5 2 5 5 3 4 3 2 5 5 4 4 2 5 4 3 4 4 1 4 1 18 1 3 3 3 4 4 3 3 3 0 3)

  ,(Ans 6 5 3 3 4 2 3 3 2 4 4 3 4 3 2 1 2 1 2 1 2 3 1 2 1 1 0 1 0 0 3 0 0 0 0 0 0 0 0 1 "Business Administration" 14 3 1 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 1 0 1 2 4 2 4 2 3 3 3 2 3 3 2 101 101 6 5 5 5 4 5 4 5 4 4 0 5 3 3 2 4 4 3 3 3 3 2 3 3 4 2 2 1 18 2 3 2 3 4 3 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 3 1 1 3 4 4 4 2 1 1 4 1 3 2 2 3 1 3 3 1 0 0 0 0 4 0 1 7 0 0 0 0 0 0 "Electrical Engineering" 15 3 101 1 0 1 1 1 0 0 1 6 3 1 1 2 3 3 1 0 0 0 0 1 1 3 2 3 1 4 4 4 2 3 1 4 3 101 101 6 5 5 5 3 5 5 5 5 5 3 1 5 4 5 3 3 5 3 5 5 3 5 3 3 3 3 1 18 1 3 4 3 3 4 3 2 0 0 1)

  ,(Ans 6 3 4 1 2 4 4 2 4 4 4 2 3 4 2 1 4 3 4 3 2 1 1 3 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Art" 15 3 101 1 0 1 1 0 0 0 101 1 3 2 2 1 1 3 1 0 0 0 1 1 0 4 2 3 3 4 2 4 2 3 3 4 2 101 101 6 5 5 5 5 5 4 5 5 5 2 4 4 4 5 3 3 5 3 3 5 3 5 4 4 4 4 1 18 1 4 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 3 4 4 4 4 4 4 4 4 3 4 4 3 3 4 4 4 3 4 3 4 4 1 1 0 1 0 4 1 1 7 0 1 1 0 0 0 "Business Administration" 15 3 3 1 0 1 0 0 0 1 1 2 3 101 101 101 3 3 1 0 1 0 1 0 1 4 4 3 3 4 3 4 3 3 3 4 3 101 101 6 5 3 3 2 5 3 2 101 3 3 3 3 4 4 5 4 4 4 3 4 3 4 4 5 4 5 1 18 2 4 4 3 4 4 3 3 1 0 1)

  ,(Ans 6 6 4 2 4 2 3 2 1 4 1 4 2 3 1 1 2 1 1 1 1 1 3 1 4 1 0 0 0 0 3 1 1 2 0 0 0 0 0 0 "Mechanical Engineering" 15 3 2 1 0 1 1 1 0 0 1 6 1 2 1 1 3 2 1 0 0 0 0 0 0 4 3 4 4 4 4 101 101 101 101 101 101 5 30 6 4 5 4 5 5 5 5 5 5 5 5 3 3 3 4 4 4 5 5 4 3 4 4 4 4 5 1 18 1 3 2 3 3 2 3 3 3 3 3)

  ,(Ans 6 5 3 3 3 4 4 4 3 1 4 4 3 1 1 4 4 1 4 4 4 4 1 4 1 1 0 0 0 0 5 1 1 7 1 0 0 0 1 0 "Physics" 16 3 2 1 0 0 0 0 0 0 1 1 1 2 2 1 3 1 1 1 0 0 0 0 0 4 4 4 3 4 4 4 3 3 2 3 3 15 30 6 5 5 5 5 5 5 4 5 5 5 5 3 4 4 5 4 5 2 4 4 3 4 4 4 4 5 1 18 2 2 4 3 4 3 2 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 1 2 4 3 3 3 3 3 4 4 2 3 3 2 4 1 2 1 1 0 1 0 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 17 3 2 1 0 1 1 0 0 1 1 2 3 101 101 101 3 1 1 0 0 0 1 0 0 4 3 3 2 2 4 3 2 101 2 2 4 15 60 6 4 4 3 5 2 1 3 0 2 0 2 3 4 3 3 3 2 3 1 3 1 3 3 4 3 4 1 18 1 4 3 101 101 101 101 101 101 101 101)

  ,(Ans 6 2 4 1 1 4 4 4 2 3 3 4 4 4 2 1 1 3 4 4 4 1 1 4 3 1 0 0 0 0 4 0 0 7 0 0 0 0 0 0 "Science" 12 3 2 0 0 1 0 0 0 0 1 2 1 1 2 1 3 1 1 1 0 1 1 0 0 3 3 4 2 1 4 4 3 4 2 1 4 20 45 6 5 5 5 5 5 2 4 2 4 5 5 3 3 2 5 5 1 2 2 4 5 3 5 5 5 5 1 18 2 3 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 2 2 2 4 3 2 4 3 4 2 3 2 1 1 1 3 1 2 1 1 2 3 0 1 0 0 0 3 0 1 0 0 0 1 0 0 0 "Health Studies" 12 3 1 1 0 1 0 0 0 0 1 6 3 101 101 101 101 3 1 0 0 0 0 0 0 4 3 1 1 1 1 4 2 2 1 1 1 101 101 6 5 5 5 3 5 5 5 5 5 5 4 2 3 3 5 5 5 5 3 3 3 5 3 4 2 2 1 18 1 2 2 3 3 4 2 3 0 0 0)

  ,(Ans 6 6 3 4 4 3 3 3 3 4 2 4 3 3 3 4 2 2 3 3 3 2 2 3 3 1 0 0 0 0 3 0 1 7 1 1 1 0 0 0 "Undecided" 13 3 2 0 0 0 1 0 1 0 1 3 101 101 101 101 3 1 0 0 0 0 0 0 0 4 4 4 3 4 4 3 4 4 3 3 3 15 60 6 5 5 4 2 5 3 5 4 4 4 4 1 1 1 5 4 3 1 3 2 3 1 3 4 1 1 0 18 2 2 1 3 3 3 3 3 3 0 0)

  ,(Ans 6 2 4 1 4 4 4 4 2 3 3 4 4 4 2 3 3 1 2 4 2 3 2 2 2 0 0 0 1 0 4 1 1 2 0 0 1 0 0 0 "Psychology" 14 3 2 1 1 1 0 0 0 0 1 2 1 3 1 1 3 1 0 1 0 0 0 0 0 4 4 4 2 2 2 4 4 2 1 1 2 10 60 6 5 5 5 1 2 2 4 1 3 2 1 3 4 2 4 3 5 5 2 2 3 2 4 5 3 3 1 18 2 2 4 2 2 3 3 3 3 0 3)

  ,(Ans 6 5 4 3 3 4 3 4 2 2 101 3 3 3 3 4 4 1 4 3 3 2 2 4 4 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Did not answer" 13 3 2 1 1 1 0 1 0 0 1 2 2 1 2 3 3 1 1 0 0 0 0 0 0 4 3 2 3 4 4 3 2 2 2 3 4 60 65 6 2 5 3 5 5 3 5 2 5 5 2 2 3 2 4 5 5 4 2 5 5 5 4 4 4 4 0 18 1 4 2 3 4 3 3 2 3 2 3)

  ,(Ans 3 5 3 2 3 4 3 3 4 4 4 3 3 2 3 1 2 1 2 2 3 3 1 1 3 1 0 0 0 0 4 0 1 0 0 0 0 1 0 0 "Undecided" 15 3 2 1 1 1 1 1 0 0 1 1 1 2 2 1 3 2 1 0 0 0 1 0 1 4 3 3 3 3 4 3 2 2 2 2 3 15 25 6 4 3 2 2 3 1 5 3 2 5 1 4 4 2 3 3 2 2 3 3 2 2 5 5 4 4 1 18 1 4 2 3 4 2 3 3 3 3 3)

  ,(Ans 6 2 4 4 4 4 4 4 4 4 3 4 1 3 1 1 2 2 2 3 3 1 101 2 4 1 0 0 0 0 3 1 0 7 1 1 1 1 0 0 "Chemistry" 12 3 2 1 0 1 1 1 0 0 1 1 1 3 2 1 2 1 1 1 0 0 0 1 0 101 101 101 101 101 101 4 4 4 3 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 1 18 2 99 99 3 4 4 3 3 0 2 0)

  ,(Ans 3 6 3 3 3 4 4 3 2 3 3 4 4 2 2 4 3 2 3 3 4 3 3 4 4 1 1 0 0 0 4 1 1 7 0 0 1 0 0 0 "Political Science" 15 3 1 1 1 1 0 1 0 0 3 6 3 101 101 101 3 1 1 1 1 0 0 1 0 3 3 3 3 3 3 3 3 3 3 3 3 20 45 6 1 1 1 1 1 1 1 1 1 1 1 3 3 2 4 4 3 3 3 3 4 3 4 4 4 4 1 18 2 1 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 3 2 4 4 4 4 4 4 4 4 1 2 3 3 2 1 2 1 1 3 1 2 1 1 0 0 0 0 3 1 0 99 0 0 0 0 0 0 "Child and Family Studies" 13 3 2 1 1 1 0 0 0 0 1 1 1 1 1 1 3 1 1 0 0 0 1 0 0 4 4 4 4 101 101 4 3 3 3 3 4 35 50 6 2 5 3 5 4 3 3 5 2 4 3 1 2 2 4 4 2 2 3 2 4 1 1 1 1 1 1 18 2 2 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 5 4 3 4 4 4 3 2 4 4 4 3 4 2 3 3 2 4 3 3 3 3 3 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Communication Studies" 14 3 2 1 0 1 1 1 0 0 1 6 1 1 3 1 3 1 1 0 0 0 1 0 0 3 3 3 3 3 3 3 3 4 3 3 4 10 20 6 3 5 3 2 5 5 5 0 5 5 2 2 3 2 4 4 4 3 2 3 3 3 4 5 3 3 0 18 2 2 2 3 4 4 3 3 1 1 1)

  ,(Ans 6 5 3 1 2 3 4 2 3 2 3 2 2 3 3 2 2 1 2 3 2 3 2 2 2 1 0 0 0 0 3 1 1 4 0 0 1 0 1 0 "Did not answer" 13 3 2 1 0 1 1 0 0 0 1 6 3 101 101 3 3 3 1 0 0 0 0 0 1 3 3 3 3 3 4 2 3 3 2 2 3 101 101 6 5 5 4 2 4 4 5 2 5 2 5 4 3 4 4 3 3 2 3 4 3 4 3 4 3 4 1 18 2 2 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 3 4 3 4 4 2 2 1 3 4 3 3 3 3 1 3 1 3 3 3 2 3 3 3 1 0 0 0 0 4 1 0 4 0 0 0 0 0 0 "English" 12 3 2 1 0 1 1 0 0 0 1 6 3 1 2 1 3 2 1 0 0 0 0 0 0 2 3 3 3 4 4 3 2 3 2 3 4 15 45 5 0 2 1 0 2 1 1 2 0 0 2 4 5 3 3 3 4 2 3 3 3 3 4 4 3 3 0 18 1 2 4 3 4 4 3 3 1 0 0)

  ,(Ans 6 3 3 2 4 4 4 3 3 4 3 3 4 4 4 1 3 1 3 4 4 2 4 3 2 1 0 0 0 0 4 1 1 4 0 0 1 0 0 0 "Art" 14 3 99 1 0 1 1 1 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 0 1 3 2 3 3 3 2 2 1 3 3 3 2 101 101 6 2 5 2 2 3 1 2 3 2 1 5 1 3 1 4 4 5 3 5 4 3 3 3 3 2 2 0 18 2 3 4 3 3 4 3 3 1 0 1)

  ,(Ans 3 5 3 1 3 4 4 4 2 4 3 4 3 1 3 1 3 1 3 1 3 1 2 3 2 1 0 0 0 0 3 1 0 4 0 0 1 0 0 0 "Undecided" 12 3 101 1 0 0 0 0 0 0 101 2 3 101 101 101 101 3 0 0 0 0 1 0 0 4 3 2 2 3 3 101 101 101 101 101 101 101 101 6 5 5 3 0 0 1 5 0 5 0 5 3 3 2 5 5 2 2 2 4 1 4 4 1 99 99 1 18 2 2 4 3 4 3 3 3 1 1 1)

  ,(Ans 5 5 4 1 3 4 4 4 2 4 2 4 4 2 2 1 2 1 1 2 1 1 1 2 2 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Psychology" 15 3 2 1 0 1 0 1 0 0 1 1 2 3 2 1 2 3 0 0 0 0 0 0 0 4 2 2 2 3 3 4 2 2 2 3 2 101 101 6 4 5 3 2 5 5 5 3 2 0 0 1 3 3 4 4 5 2 2 3 3 3 4 5 99 99 1 18 1 2 1 3 3 3 2 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 2 4 1 1 4 3 2 3 1 2 3 1 2 1 1 1 1 1 1 1 0 0 0 0 1 1 0 2 0 1 1 0 0 0 "Music" 17 3 2 1 1 1 1 1 0 1 1 3 2 2 2 1 3 1 0 0 0 0 0 0 0 3 4 4 3 3 3 3 3 4 3 3 2 15 45 6 5 5 4 5 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 1 18 1 2 2 3 4 2 3 2 3 3 3)

  ,(Ans 6 6 3 3 2 3 4 3 2 3 4 4 2 3 2 3 2 1 3 2 3 1 1 4 4 1 0 0 0 0 3 0 101 0 0 1 1 0 0 0 "Health Studies" 10 3 2 1 0 1 0 1 0 1 1 6 1 3 2 1 3 1 0 0 0 1 0 0 0 4 4 4 3 3 3 4 3 3 2 2 2 15 15 6 5 5 4 5 3 4 5 4 5 5 3 3 3 2 5 5 4 3 3 3 2 3 3 3 2 3 1 18 2 2 1 3 3 3 3 3 3 0 3)

  ,(Ans 3 3 3 101 4 3 3 3 2 3 4 3 4 2 2 4 3 1 4 3 3 2 1 2 1 1 0 0 0 0 4 0 1 0 0 0 1 0 1 0 "Undecided" 17 3 101 1 0 1 1 1 0 0 1 1 3 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 2 4 4 2 2 2 1 3 2 3 50 6 5 5 5 0 3 2 2 2 5 3 3 4 3 3 3 2 4 1 2 3 2 4 3 3 4 3 0 18 1 3 5 3 4 4 3 4 3 3 3)

  ,(Ans 3 6 3 1 4 4 4 3 3 4 4 4 4 4 3 1 3 1 3 2 3 2 1 4 3 1 0 0 0 0 2 1 1 0 1 1 1 0 0 0 "Undecided" 12 3 101 0 0 0 0 0 0 0 1 1 3 101 101 101 101 3 1 0 0 0 0 0 0 101 101 101 101 101 101 4 4 4 4 3 4 101 101 6 0 1 1 2 5 1 4 0 4 0 2 1 2 2 3 3 1 1 1 2 2 1 4 4 3 99 0 18 2 4 2 3 4 3 3 3 3 3 3)

  ,(Ans 3 6 3 2 4 4 4 3 3 4 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 2 0 0 0 0 1 0 0 0 0 "Computer Engineering" 14 3 2 1 0 1 1 0 0 0 3 3 3 1 2 1 2 3 1 0 0 0 0 0 0 3 4 3 2 3 2 3 4 3 2 3 2 101 101 6 5 5 5 2 2 2 5 3 3 3 2 2 2 2 2 2 2 2 2 2 2 101 3 3 3 3 0 18 1 1 1 2 2 2 2 2 0 0 0)

  ,(Ans 6 4 4 3 4 4 4 4 3 2 4 3 4 4 4 1 3 1 3 3 4 2 1 4 4 1 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Art" 14 3 2 1 0 0 0 0 0 0 1 3 3 1 1 3 3 3 1 0 0 0 1 1 0 4 4 3 3 4 3 4 3 3 3 3 2 101 101 6 5 5 5 5 5 5 5 5 5 5 4 3 3 3 5 5 5 4 101 3 4 3 5 5 5 5 1 18 1 4 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 2 4 3 1 1 2 3 1 4 2 2 1 2 1 2 2 1 1 1 2 1 1 1 1 0 0 5 0 1 0 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 1 3 1 1 2 2 3 0 0 0 0 0 0 0 4 4 4 4 4 101 2 2 2 2 2 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 3 4 4 4 5 4 3 4 3 4 4 3 3 3 1 18 1 5 5 3 3 2 2 2 3 3 3)

  ,(Ans 3 5 3 2 4 4 3 2 4 3 4 3 3 3 2 2 2 2 3 3 2 2 2 3 4 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Undecided" 13 3 2 1 1 1 1 0 0 0 1 6 1 2 1 1 3 1 1 0 0 1 1 1 0 4 4 4 3 3 3 3 3 3 2 3 2 15 30 6 5 5 4 4 4 5 5 4 5 5 5 4 4 3 3 4 5 5 3 5 3 5 3 3 3 4 1 18 2 3 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 4 4 3 4 4 4 2 2 3 3 4 4 4 4 4 3 1 4 3 3 2 3 1 4 1 0 0 0 0 5 1 0 7 0 0 0 1 0 0 "English" 15 3 2 1 0 1 1 0 0 0 1 3 3 1 1 1 3 1 1 1 0 1 0 1 0 4 4 4 2 4 4 1 1 4 3 4 1 20 90 6 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 4 3 2 4 5 5 5 4 5 5 5 1 18 2 3 3 3 4 1 3 3 3 0 3)

  ,(Ans 5 6 2 2 4 4 3 2 2 4 4 4 4 2 1 1 2 2 3 3 3 3 3 3 4 1 0 0 1 0 4 1 1 4 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 0 1 0 0 1 6 1 3 1 1 1 1 0 0 0 1 0 0 0 1 1 3 3 1 3 1 3 3 3 1 3 15 15 6 101 5 5 4 5 5 5 3 4 3 3 3 4 3 5 5 5 3 2 3 3 4 4 5 4 4 1 18 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 1 4 4 3 3 4 3 1 3 3 3 2 1 3 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "English" 12 3 101 1 0 1 1 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 1 4 2 4 3 4 4 3 2 2 1 4 4 101 101 6 5 5 5 1 4 4 3 4 4 1 5 5 5 5 3 2 3 2 3 5 4 5 4 4 3 4 0 18 2 2 2 3 3 2 3 3 3 0 3)

  ,(Ans 101 3 4 3 3 4 4 3 4 3 3 3 4 4 4 3 3 2 3 3 4 3 3 3 3 1 0 1 0 0 4 1 1 0 1 1 0 0 0 0 "Business Administration" 101 3 2 1 0 1 1 1 0 0 1 6 2 101 101 3 3 1 1 0 0 0 0 0 0 3 3 3 2 4 3 3 3 3 101 3 101 101 101 6 4 5 3 3 1 5 5 3 3 1 5 2 2 2 3 3 4 4 2 2 2 5 3 3 4 4 1 18 2 2 2 3 4 2 3 3 3 0 3)

  ,(Ans 6 2 4 3 4 4 4 4 3 4 3 3 3 3 4 3 3 1 4 3 3 3 2 4 3 1 0 1 0 0 3 1 1 0 1 0 0 0 0 0 "Health Studies" 14 3 2 1 0 0 1 1 0 0 1 2 3 2 1 1 3 1 0 1 0 0 0 0 0 4 3 3 3 3 3 4 3 2 1 2 1 40 60 6 5 5 4 5 5 3 5 2 4 1 5 3 3 2 4 4 3 4 3 3 2 3 3 4 4 4 1 18 2 3 2 3 4 4 2 3 3 0 3)

  ,(Ans 6 3 4 4 4 4 4 4 1 2 4 3 2 1 1 1 1 3 4 4 2 1 1 3 1 1 0 0 0 0 1 1 1 4 0 0 1 0 0 0 "Health Studies" 101 3 2 1 0 1 1 1 0 0 1 1 3 1 1 2 3 3 1 0 0 0 0 0 0 3 3 4 4 2 1 101 101 101 1 1 1 101 101 6 5 5 4 3 3 5 5 3 5 0 5 3 1 2 4 4 2 4 3 101 3 2 4 4 4 99 0 18 1 2 2 3 3 4 3 3 3 0 3)

  ,(Ans 5 3 3 1 1 4 4 4 3 4 3 4 4 2 3 4 4 1 3 3 4 4 2 2 4 1 0 1 0 0 4 1 0 0 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 6 2 1 2 3 3 1 1 1 0 0 0 0 0 4 4 4 4 4 4 4 3 3 3 4 4 3 20 6 5 5 5 5 5 5 5 3 5 5 5 4 4 4 5 5 4 3 4 5 3 3 5 5 5 5 1 18 2 2 2 3 4 3 3 3 3 2 3)

  ,(Ans 6 5 2 1 4 4 4 4 3 4 4 3 4 3 2 1 2 1 3 1 1 1 3 3 1 1 0 0 0 0 4 1 0 0 0 0 0 1 0 0 "Film" 12 3 2 1 1 0 0 1 0 0 1 1 3 1 1 3 3 2 1 0 0 0 0 0 0 4 4 4 3 3 4 4 3 2 2 2 4 1 15 6 5 5 4 3 4 3 5 5 5 5 5 4 4 2 5 4 4 4 4 5 4 5 5 4 3 99 1 18 2 5 3 3 4 3 3 2 3 3 3)

  ,(Ans 6 3 4 3 1 4 4 4 4 4 4 2 2 3 2 4 3 2 3 3 3 2 3 4 4 0 1 0 0 0 4 1 1 0 0 0 0 0 1 0 "Business Administration" 16 3 3 1 0 1 1 1 0 0 1 6 3 3 2 1 3 1 1 0 0 0 0 0 0 3 3 3 2 3 3 101 101 101 101 101 101 5 15 6 3 5 5 4 5 4 4 3 4 5 5 3 3 2 2 2 2 2 3 3 3 2 3 3 3 5 1 18 1 2 101 3 4 3 3 3 0 0 0)

  ,(Ans 3 3 3 2 4 4 4 4 4 4 4 4 4 3 2 2 3 2 3 2 3 3 3 4 2 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Undecided" 16 3 2 1 0 1 1 1 0 0 1 1 1 2 1 1 3 1 1 1 0 0 1 1 0 4 4 3 3 3 3 4 3 2 2 3 3 15 40 6 5 5 5 3 5 2 5 3 2 3 5 4 5 4 5 5 3 4 3 4 3 3 4 4 5 5 1 18 2 3 3 3 4 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 3 4 4 4 1 2 1 3 3 3 1 1 3 1 1 0 0 1 0 5 1 1 99 0 0 0 0 0 1 "Did not answer" 13 3 99 1 0 0 0 0 0 0 1 3 3 1 1 2 2 3 1 1 1 0 0 0 1 4 4 4 3 4 3 4 2 2 2 3 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 4 3 5 4 4 4 4 4 5 4 5 5 4 5 1 18 2 4 5 3 4 3 3 3 1 2 1)

  ,(Ans 6 6 4 3 3 4 4 2 3 3 4 3 3 3 3 2 1 1 1 1 3 3 1 3 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Health Studies" 15 3 3 1 0 1 0 1 0 0 1 1 3 2 1 1 1 3 0 0 0 0 1 0 1 4 3 3 3 3 3 4 3 3 3 3 2 101 101 6 2 3 1 0 3 1 2 2 2 4 0 1 1 1 4 4 2 1 3 3 3 4 3 3 3 3 0 18 1 5 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 6 3 1 4 4 4 2 3 3 2 4 3 3 2 1 4 1 4 2 2 1 3 3 3 1 0 0 0 0 2 1 101 7 1 1 1 0 0 0 "Undecided" 13 3 1 1 0 1 0 0 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 0 3 3 3 3 4 4 3 3 3 3 4 4 101 101 101 0 3 2 1 1 2 3 2 1 2 0 1 1 1 1 1 1 1 1 1 1 2 4 3 3 3 0 18 2 2 2 3 4 3 3 2 3 2 3)

  ,(Ans 6 6 4 3 2 101 3 4 2 4 3 3 3 2 1 1 3 1 3 3 4 2 2 3 4 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Business Administration" 14 3 2 1 0 1 1 1 0 0 1 3 3 1 1 3 3 3 1 1 0 0 0 1 1 4 4 4 3 4 2 4 4 101 3 4 2 101 101 6 5 5 5 5 5 4 5 5 5 5 3 3 4 4 3 3 4 4 4 4 4 4 2 5 5 101 0 18 2 2 3 3 3 2 3 3 3 2 3)

  ,(Ans 6 3 3 1 4 4 3 3 1 4 4 2 2 1 1 1 3 1 3 1 4 3 1 3 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Business Administration" 14 3 2 1 1 1 1 1 0 0 1 1 2 2 2 1 2 2 1 0 0 0 0 0 0 101 101 101 101 101 101 4 4 4 3 4 4 8 20 6 5 5 5 5 5 5 5 5 5 5 5 4 4 2 3 3 3 5 3 3 5 4 5 4 3 4 1 18 2 4 3 3 3 3 3 3 3 3 3)

  ,(Ans 5 5 2 1 4 3 4 2 3 4 4 2 3 3 4 3 3 2 3 2 3 3 2 4 2 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Psychology" 13 3 2 1 1 1 0 0 0 0 1 1 1 1 2 1 3 1 0 0 0 0 1 0 0 3 2 3 2 2 2 4 2 2 1 1 2 2 30 6 5 5 5 3 5 5 5 3 3 3 5 3 3 4 4 4 3 5 2 3 3 4 4 4 2 2 1 18 2 3 99 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 4 3 4 4 4 3 3 3 3 4 4 3 3 1 3 1 3 3 3 2 2 3 4 1 0 0 0 0 3 0 1 7 1 0 0 0 0 0 "Undecided" 16 3 3 1 0 1 1 0 0 0 1 6 3 1 1 2 2 3 1 1 0 0 1 1 0 1 101 101 101 101 101 4 4 4 4 4 4 101 101 6 5 5 5 4 5 4 5 3 4 4 5 3 4 2 5 5 3 2 2 5 3 5 5 5 3 3 1 18 2 2 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 3 4 4 4 3 1 2 4 3 4 4 3 2 2 1 4 3 4 1 3 4 3 0 0 1 0 0 3 1 1 7 0 1 0 1 0 0 "Environmental Sciences and Resources" 12 3 2 1 1 0 0 1 0 0 1 1 3 2 1 2 2 2 1 0 0 0 0 1 0 4 3 2 3 3 3 3 2 2 3 2 3 0 7 6 3 4 4 5 5 5 5 4 5 5 5 3 2 3 5 5 5 4 4 4 5 4 5 5 4 5 1 18 2 2 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 6 4 4 4 2 1 3 3 3 2 1 1 4 1 4 2 1 4 1 4 3 1 2 2 1 0 0 0 0 4 0 1 2 0 0 0 0 0 1 "Electrical Engineering" 13 3 2 1 0 1 0 0 0 0 1 3 101 2 101 101 3 1 0 1 0 0 0 0 0 3 3 1 2 3 4 3 2 1 2 3 4 15 20 6 3 3 2 5 5 5 4 4 4 3 5 1 2 2 5 5 5 1 5 4 3 101 5 5 4 4 1 18 1 3 4 3 4 101 2 3 0 0 0)

  ,(Ans 6 2 4 4 4 4 3 4 2 4 2 4 3 4 2 4 4 1 4 1 3 3 1 3 1 1 0 0 0 0 3 1 0 0 0 0 0 0 0 0 "Business Administration" 13 3 2 1 0 0 1 1 0 0 1 6 1 1 3 1 2 1 1 1 0 0 0 0 0 4 3 4 2 4 3 4 3 4 2 4 2 20 15 6 2 5 3 3 5 1 4 2 3 2 5 4 4 4 5 5 3 5 3 4 4 5 5 5 5 5 1 18 2 4 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 4 1 3 2 4 1 2 2 3 2 2 4 4 1 2 2 3 1 3 1 1 1 1 1 1 0 0 0 4 1 0 0 0 0 1 1 0 0 "Civil Engineering" 16 3 101 1 0 1 0 0 0 0 1 3 3 1 1 2 2 3 1 0 0 0 1 0 0 3 2 2 2 3 2 4 2 3 2 3 2 101 101 6 2 5 5 5 2 5 5 2 4 0 5 3 3 3 3 3 5 5 2 5 2 3 3 4 99 3 0 18 1 5 4 3 4 4 2 3 1 2 1)

  ,(Ans 6 6 4 4 4 4 4 101 3 3 3 3 1 4 4 4 4 1 4 3 4 3 1 3 4 1 1 0 1 0 2 1 1 7 1 1 1 0 0 0 "Psychology" 13 3 2 0 0 0 0 0 0 0 2 6 3 3 3 2 2 1 1 1 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 35 45 6 2 1 0 3 5 0 3 4 0 2 4 2 1 1 3 3 2 2 2 2 2 2 4 5 5 5 1 18 1 1 1 3 3 101 3 3 0 0 1)

  ,(Ans 101 3 3 4 4 3 4 4 1 4 3 3 3 1 3 1 3 1 3 3 3 1 3 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "English" 13 3 99 1 0 0 0 0 0 0 1 6 3 1 2 2 3 3 1 1 0 0 0 0 0 4 4 4 3 4 4 3 2 4 3 4 2 101 101 6 5 5 5 5 5 5 5 5 5 5 4 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 1 18 2 2 1 3 3 3 3 3 1 2 1)

  ,(Ans 3 6 3 2 3 3 3 3 4 4 3 3 2 2 3 1 2 3 3 1 2 2 1 1 2 1 0 0 0 0 3 0 0 0 1 0 0 0 0 0 "Undecided" 101 3 2 1 0 0 0 0 0 0 1 1 2 3 1 1 1 2 0 0 0 0 1 0 0 3 3 101 3 3 3 101 101 101 101 101 101 24 40 5 2 1 2 3 2 3 2 2 2 4 4 2 2 2 2 2 3 2 1 2 2 2 4 4 3 4 1 18 1 3 2 2 2 2 2 2 1 0 1)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 1 4 4 4 3 3 1 4 3 3 1 1 3 1 1 0 0 0 0 4 0 1 0 1 0 0 0 0 0 "Accounting" 13 3 2 1 0 1 1 1 0 0 1 6 3 1 1 1 3 1 1 0 0 1 0 0 0 1 1 1 1 1 1 1 1 1 3 1 1 30 70 6 5 5 2 5 5 5 5 1 5 5 5 3 4 3 5 5 3 3 3 3 4 4 4 5 4 5 1 18 2 2 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 4 4 1 3 4 4 4 2 3 3 4 2 3 4 1 4 1 3 2 4 3 3 2 4 1 0 0 1 0 4 0 1 7 1 0 1 0 0 0 "Psychology" 13 3 99 1 0 1 1 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 1 0 101 101 101 101 101 101 3 3 3 3 4 3 101 101 6 5 5 5 3 3 2 3 2 3 2 4 2 2 2 4 3 4 3 2 3 3 3 4 5 4 4 1 18 2 4 4 3 4 4 3 3 0 0 0)

  ,(Ans 3 6 3 2 4 4 4 3 2 2 4 3 2 3 3 1 4 1 4 4 4 4 1 4 4 0 0 0 1 0 4 0 1 7 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 0 0 0 0 0 1 6 1 2 101 101 3 2 1 1 0 0 0 0 0 4 4 4 4 3 3 3 3 3 3 2 2 9 30 6 3 3 4 5 5 1 4 1 5 1 4 2 4 4 4 4 2 2 4 4 4 3 5 5 5 5 1 18 2 1 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 3 4 4 4 2 3 4 3 4 3 2 2 3 2 2 3 2 1 1 2 2 2 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 0 0 0 0 0 1 1 101 3 2 101 101 1 1 0 0 0 0 0 0 3 101 101 101 3 3 3 3 4 3 101 101 20 30 6 3 3 3 4 4 5 5 5 5 5 5 5 5 5 5 5 4 4 5 5 4 5 5 5 5 5 1 18 1 2 3 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 2 2 4 3 3 4 1 3 1 4 1 4 1 3 4 3 3 4 1 0 0 0 0 2 0 0 0 0 0 1 0 0 0 "Undecided" 12 3 2 1 0 1 0 0 0 0 1 3 3 1 1 1 1 2 0 0 0 0 0 0 0 3 3 3 2 4 3 3 2 3 2 4 3 101 15 6 5 5 5 5 5 4 5 4 5 5 5 3 4 2 4 3 3 3 3 4 4 3 3 4 3 3 1 18 2 2 3 3 3 2 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 2 2 4 3 3 4 1 4 3 3 3 3 3 3 3 4 2 1 0 0 0 0 3 1 1 2 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 1 0 0 0 1 1 1 3 1 1 1 1 0 1 0 0 0 0 0 4 3 3 3 3 3 4 3 3 3 3 3 10 30 6 0 3 5 5 5 5 5 5 5 3 5 2 4 4 3 3 5 3 3 3 3 4 3 4 3 4 1 18 2 2 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 2 4 4 4 4 4 3 3 2 3 4 3 1 3 1 3 3 3 2 1 1 4 1 1 0 0 0 0 3 1 1 7 0 0 0 0 0 0 "Economics" 13 3 2 1 0 0 0 0 0 0 1 2 3 101 101 101 101 3 0 1 0 0 0 0 0 101 4 101 101 101 2 3 101 2 3 3 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 4 4 3 4 4 5 4 4 4 101 3 3 1 18 2 5 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 5 4 1 1 4 4 4 3 3 2 3 3 4 3 4 4 1 4 2 4 3 2 2 4 1 0 0 0 0 3 1 1 0 1 0 1 0 0 0 "Undecided" 15 3 2 1 0 1 1 1 0 0 1 3 1 3 1 1 1 1 0 1 0 0 0 0 0 4 3 3 2 4 4 3 3 3 1 4 4 30 45 6 5 5 5 5 5 5 5 5 5 4 5 4 4 3 4 5 4 2 4 4 3 4 3 4 3 4 0 18 2 2 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 2 4 1 3 4 4 4 2 3 2 1 4 4 2 4 3 1 4 3 2 2 1 2 2 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Biology" 15 3 2 1 0 1 0 0 0 0 1 1 1 3 1 1 2 1 1 1 0 0 0 0 0 4 3 3 3 3 4 4 3 2 3 2 4 100 30 6 5 5 5 5 5 5 5 4 5 5 5 3 4 3 4 4 3 3 3 3 3 3 5 5 5 4 1 18 2 3 2 3 4 4 3 2 1 1 1)

  ,(Ans 6 6 4 3 3 4 4 4 4 3 3 4 1 4 3 4 1 1 4 4 3 2 1 4 4 1 0 0 0 0 3 0 1 7 0 1 0 0 0 0 "Did not answer" 13 3 2 0 0 1 1 1 0 0 1 3 101 101 101 101 3 1 1 1 0 0 0 0 0 4 4 4 2 4 2 4 3 4 2 4 2 50 120 5 3 5 2 1 5 3 4 1 3 2 5 3 2 2 4 4 3 3 3 3 5 3 4 5 4 5 1 18 2 1 1 3 3 3 3 3 1 1 1)

  ,(Ans 5 3 3 3 4 4 4 4 4 4 4 4 2 4 4 4 3 2 4 2 3 2 3 2 4 1 0 0 0 0 4 1 1 99 0 0 0 0 0 1 "Undecided" 101 3 101 1 0 1 0 0 0 0 1 6 101 3 101 101 2 1 1 1 0 0 0 0 0 4 4 4 4 3 4 4 3 4 3 3 4 9 25 6 5 5 4 5 4 4 2 5 5 5 1 3 3 2 4 4 2 2 2 4 4 3 4 5 2 2 1 18 1 3 2 3 3 2 3 2 1 2 1)

  ,(Ans 6 6 4 3 4 4 4 4 3 3 4 3 3 3 3 3 3 3 2 2 2 2 1 3 3 0 0 0 0 1 2 1 1 0 0 0 1 0 0 0 "Undecided" 14 3 2 1 0 0 0 0 0 0 1 3 101 2 101 101 3 1 0 0 0 0 1 0 0 3 2 3 3 3 3 3 2 3 3 3 3 20 25 6 2 5 5 5 5 2 3 5 5 5 5 2 3 3 4 3 3 3 3 3 4 4 5 5 4 4 1 18 2 3 4 3 4 3 3 3 1 0 1)

  ,(Ans 5 6 2 4 4 4 4 4 3 4 3 3 4 3 3 1 3 1 2 2 2 3 4 4 1 1 0 0 1 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 0 0 0 0 0 1 1 3 1 2 1 2 2 0 0 0 0 1 0 0 3 3 2 2 3 4 2 2 2 2 3 4 1 5 6 4 4 3 4 4 4 4 4 4 4 4 3 3 3 3 4 3 3 3 3 3 3 4 4 3 3 1 18 1 2 4 3 3 4 3 3 1 1 1)

  ,(Ans 6 4 2 1 4 4 4 4 3 4 3 4 4 3 4 3 3 2 4 1 4 2 1 3 1 1 0 0 0 0 3 1 1 0 1 1 1 0 0 1 "Undecided" 13 3 2 1 0 1 0 0 0 0 1 1 3 1 1 2 3 1 1 1 1 0 0 1 0 4 4 4 4 4 4 3 3 101 3 3 3 20 45 5 4 4 4 5 5 3 5 2 5 5 3 3 3 4 5 4 2 4 2 3 2 3 5 5 5 5 1 18 1 3 3 3 3 2 2 3 1 2 1)

  ,(Ans 5 3 3 1 4 4 3 2 2 4 4 3 3 2 3 4 2 1 2 4 2 3 1 4 4 0 0 0 1 1 2 1 0 7 0 0 0 0 0 0 "Business Administration" 13 3 2 1 0 1 0 1 0 0 1 6 1 2 1 1 2 1 0 0 0 0 0 0 0 4 4 4 1 4 4 3 2 2 1 4 4 20 30 6 3 5 5 5 5 5 5 2 5 4 5 3 4 3 3 5 2 2 3 3 2 3 3 3 2 3 1 18 2 3 2 3 4 2 1 3 1 2 1)

  ,(Ans 6 3 4 3 3 4 4 1 2 4 3 1 3 4 4 1 3 2 4 4 3 1 1 1 1 1 0 0 0 0 3 1 1 1 0 0 0 1 0 0 "Business Administration" 14 3 2 1 0 1 0 0 0 0 3 1 3 101 101 101 101 3 1 1 0 0 1 1 1 3 4 4 2 3 3 101 101 101 101 101 101 101 101 6 1 3 3 0 4 3 2 2 2 3 0 2 2 2 2 2 2 2 1 2 2 2 4 4 4 4 1 18 2 2 1 2 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 1 3 4 4 2 2 2 3 3 4 3 3 1 2 1 3 2 2 1 4 4 3 1 0 0 0 0 5 1 1 7 0 0 0 0 0 1 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 6 1 1 1 2 3 2 0 1 0 0 0 1 0 4 3 3 3 3 4 3 2 3 1 2 3 3 30 6 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 4 4 3 4 5 5 5 5 5 5 4 1 18 2 5 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 3 3 2 4 4 4 3 1 2 3 4 3 3 2 1 3 1 3 3 3 2 1 4 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Secondary Education" 16 3 2 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 1 1 1 0 0 0 0 3 3 2 2 2 2 3 3 2 2 2 2 101 101 6 5 5 4 4 5 5 5 2 2 5 4 3 3 3 4 4 3 4 3 3 4 4 4 4 4 4 1 18 2 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 3 3 4 4 3 4 4 4 4 4 3 4 4 4 3 4 4 4 3 3 4 4 4 1 0 0 0 0 3 1 0 4 0 0 1 0 0 0 "Undecided" 13 3 3 1 0 1 1 0 0 0 101 2 1 2 2 1 3 1 1 0 0 0 0 0 0 3 3 3 2 3 3 3 3 2 2 3 3 20 60 6 2 2 2 3 4 2 2 4 3 1 4 3 3 3 3 4 2 2 3 3 2 3 3 1 2 2 1 18 1 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 3 4 4 2 4 3 2 1 3 2 1 1 1 3 2 2 1 3 3 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Chemistry" 15 3 3 1 0 0 0 0 0 0 1 2 3 1 1 2 2 3 0 0 0 0 0 0 1 4 3 3 3 3 3 4 3 2 1 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 1 2 3 4 4 4 1 3 3 4 4 3 4 3 3 1 18 2 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 4 4 4 4 3 4 4 3 1 4 2 4 4 4 4 4 4 1 1 0 0 0 0 4 1 0 0 0 0 0 0 0 1 "Psychology" 14 3 101 1 0 1 0 0 0 0 1 1 3 2 1 2 2 3 0 0 0 0 1 0 1 4 4 4 2 3 4 4 3 3 1 2 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 5 5 5 4 3 4 4 4 5 5 5 5 1 18 2 5 5 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 4 1 1 4 3 3 2 2 3 3 4 4 2 1 2 1 2 2 2 2 1 3 4 1 0 0 0 0 4 1 1 0 0 1 1 0 0 0 "Did not answer" 13 3 101 1 0 1 1 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 1 4 4 3 2 4 3 3 3 3 1 4 4 101 101 6 3 1 2 3 4 1 5 2 4 5 3 2 3 2 5 5 2 5 3 4 5 4 5 3 4 4 1 18 1 5 4 3 4 4 2 3 3 2 3)

  ,(Ans 3 5 3 1 2 4 3 1 2 2 4 2 2 1 1 2 1 1 1 2 2 1 1 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Music" 12 3 101 1 0 1 1 1 0 0 1 1 3 1 1 2 1 3 0 0 0 0 0 0 1 4 3 3 2 3 1 4 3 3 2 4 1 101 101 6 5 5 5 5 5 4 4 3 5 5 5 3 2 2 4 4 3 3 3 4 3 4 4 4 4 4 0 18 1 3 3 3 3 2 3 3 3 0 3)

  ,(Ans 3 5 3 1 3 3 3 3 2 3 3 3 3 3 3 2 2 1 2 2 3 3 2 3 3 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "Music" 15 3 2 1 0 1 0 0 0 0 1 1 3 1 1 2 2 3 1 1 0 0 0 0 0 4 3 4 3 4 4 3 2 3 2 2 2 101 101 6 5 5 5 5 5 5 5 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 2 2 2 2 1 18 2 4 4 3 3 2 3 3 3 2 3)

  ,(Ans 6 5 4 3 101 4 4 4 3 3 4 2 3 4 2 1 2 2 3 3 4 2 1 3 3 1 0 0 0 0 5 1 1 0 0 0 1 0 0 0 "Philosophy" 15 3 2 1 0 1 1 1 0 0 1 1 3 1 2 1 2 3 1 0 1 0 1 0 1 4 4 3 3 4 4 3 4 3 2 4 4 101 101 6 5 5 5 3 5 4 5 3 5 4 5 4 4 4 5 5 2 3 2 4 3 4 4 4 3 4 0 18 2 3 3 3 2 3 3 3 3 2 3)

  ,(Ans 6 6 3 1 3 3 3 1 3 3 3 4 2 2 1 4 2 1 2 1 2 1 1 3 1 1 0 0 0 0 3 0 1 7 0 0 0 0 0 0 "Undecided" 16 3 2 1 0 1 0 0 0 0 1 1 1 3 1 1 1 1 1 1 0 0 1 0 0 3 3 3 3 3 3 4 1 1 1 3 3 28 45 6 5 5 5 2 5 4 5 4 4 1 1 3 3 3 4 4 4 3 3 3 3 3 4 3 3 4 1 18 1 3 2 2 2 2 2 2 3 2 0)

  ,(Ans 99 2 4 4 4 4 3 4 3 4 3 4 2 3 2 4 3 2 2 3 3 2 2 1 3 1 0 0 0 0 5 1 1 2 0 0 0 0 0 0 "" 12 3 2 0 0 1 1 1 0 0 1 3 3 2 3 1 2 2 1 0 1 0 0 0 0 4 1 3 2 4 2 3 1 4 2 4 3 20 25 6 5 5 5 5 5 5 5 5 5 5 5 1 1 1 1 1 1 5 4 4 4 5 5 5 5 5 1 18 1 3 4 3 2 1 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 3 3 4 3 4 4 1 4 3 4 4 3 1 3 2 3 1 1 3 1 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Business Administration" 17 3 2 1 1 1 1 0 0 0 1 1 3 1 1 2 2 2 0 0 0 0 0 0 0 4 4 3 3 3 3 4 4 2 2 2 3 1 5 99 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 5 2 3 4 4 5 5 5 5 5 1 18 1 2 4 3 3 4 3 3 1 1 1)

  ,(Ans 3 3 3 1 4 4 4 2 3 4 3 4 4 3 3 4 3 2 4 3 3 2 3 4 4 1 0 0 0 0 3 1 0 7 0 1 1 0 0 0 "Biology" 12 3 2 1 0 1 0 0 0 0 1 6 3 3 2 1 3 1 1 0 0 0 0 0 0 4 3 4 1 4 2 4 3 4 1 3 1 25 90 6 5 5 5 4 4 2 4 4 5 5 5 3 4 3 4 4 3 2 3 4 3 4 4 4 3 5 1 18 2 1 1 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 4 4 3 4 4 4 2 4 2 4 4 3 3 1 4 2 1 0 0 0 0 3 0 0 0 0 0 1 0 0 0 "Environmental Sciences and Resources" 15 3 2 1 0 1 0 0 0 0 1 2 2 2 2 1 3 1 1 1 0 0 0 1 0 4 4 4 3 4 3 4 3 4 3 3 3 20 45 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 5 5 5 5 5 5 5 1 18 2 2 3 3 4 2 3 3 3 3 3)

  ,(Ans 6 3 4 4 4 3 3 3 3 4 3 3 2 4 3 4 3 3 3 3 3 3 4 3 3 1 0 0 1 0 4 1 1 7 0 0 1 0 0 0 "Computer Engineering" 15 3 2 1 1 1 1 0 0 0 1 3 2 3 101 101 3 1 1 0 0 0 1 0 0 101 101 101 101 101 101 101 101 101 101 101 101 15 25 6 4 3 3 4 4 2 3 3 3 3 4 101 101 101 101 101 101 101 101 101 101 101 4 4 4 4 1 18 1 2 4 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 2 2 3 4 4 1 1 1 4 4 3 1 1 1 1 1 1 1 3 1 3 4 4 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Undecided" 14 3 2 0 0 0 1 1 0 0 1 3 101 101 101 3 3 2 1 1 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 7 15 6 2 3 5 4 4 1 5 4 5 5 5 3 4 1 4 4 2 4 3 3 4 3 5 5 5 5 1 18 2 5 2 3 3 2 2 2 3 2 3)

  ,(Ans 6 5 1 2 2 3 3 3 3 4 2 3 3 3 2 4 2 3 2 2 2 3 1 1 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Health Studies" 6 3 2 0 0 1 1 1 0 0 1 2 2 1 2 2 3 1 0 0 0 0 0 0 0 4 4 2 3 2 3 3 3 2 3 2 2 15 30 6 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 1 3 4 3 4 3 3 3 1 0 1)

  ,(Ans 3 4 3 1 3 4 3 4 2 3 3 3 4 4 3 1 2 1 3 2 2 2 2 3 1 1 0 0 0 0 2 0 1 0 0 0 1 0 0 0 "Did not answer" 16 3 99 1 0 1 0 0 0 1 1 1 3 1 2 1 2 3 1 0 0 0 0 0 0 4 3 4 3 3 4 3 3 4 2 4 3 101 101 6 5 5 5 5 5 3 4 5 5 5 4 5 3 5 4 5 2 5 4 5 3 5 3 4 3 5 1 18 1 4 5 3 3 4 3 3 1 1 1)

  ,(Ans 6 5 4 1 4 4 3 1 2 3 4 4 4 3 3 3 2 1 2 2 3 3 2 4 3 1 0 0 0 0 2 0 1 7 0 0 1 0 0 1 "Undecided" 12 3 99 1 0 1 0 0 0 0 1 6 101 101 101 101 3 1 1 1 0 0 0 0 0 4 4 4 3 4 3 4 2 4 2 4 2 8 60 6 1 5 4 2 5 0 5 0 4 3 0 2 4 3 4 5 2 4 1 3 4 3 4 5 3 3 1 18 1 3 99 3 4 4 2 3 1 2 1)

  ,(Ans 6 2 4 4 4 3 4 4 2 4 4 3 4 3 3 4 3 3 4 4 4 3 1 2 2 1 0 0 0 0 3 0 1 7 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 2 1 2 3 1 1 1 1 0 0 0 0 0 0 4 4 4 2 4 4 3 3 3 2 3 4 101 15 6 5 5 5 4 5 5 5 5 5 5 4 4 5 4 3 4 5 4 3 4 5 4 4 4 4 4 1 18 2 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 3 1 4 3 3 3 4 4 4 3 3 3 1 4 3 2 3 3 4 4 4 4 1 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Undecided" 16 3 2 1 1 1 1 1 0 0 1 1 1 2 3 1 1 1 1 0 0 0 0 0 0 4 2 2 3 3 3 3 3 3 3 3 3 15 30 6 2 5 4 5 5 3 4 3 2 3 1 2 3 3 4 4 2 4 3 4 3 3 4 4 4 5 1 18 1 2 5 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 4 3 4 4 4 3 4 4 4 2 2 4 2 4 3 3 3 3 3 3 3 1 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Accounting" 14 3 2 1 0 0 0 0 0 0 1 1 1 2 1 1 3 1 1 0 0 0 0 0 0 4 4 4 4 4 4 4 3 4 2 3 4 25 60 6 0 2 3 0 3 0 5 1 1 0 2 1 1 1 1 1 1 2 1 1 1 1 4 4 4 4 1 18 1 2 5 3 3 2 2 2 0 2 1)

  ,(Ans 6 3 4 1 4 4 4 4 2 3 4 4 4 4 4 1 3 1 2 2 2 3 2 4 4 1 0 0 0 0 4 1 1 7 0 0 1 1 0 0 "Business Administration" 12 3 2 1 1 1 1 1 0 0 1 2 2 1 1 2 3 1 1 0 0 0 0 0 0 4 4 4 4 4 4 4 3 4 3 3 3 30 25 6 5 5 5 3 5 5 5 5 5 5 5 5 5 4 4 4 4 5 3 3 2 2 5 5 5 5 1 18 1 4 4 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 4 3 4 2 2 1 3 4 3 3 4 4 2 1 3 1 3 2 2 3 2 3 3 1 0 0 0 1 4 1 1 7 0 1 1 0 0 0 "Did not answer" 15 3 3 1 0 1 0 1 0 0 1 2 3 1 1 1 2 3 1 0 0 0 0 0 0 4 3 2 2 3 4 3 3 3 1 2 3 101 101 6 3 4 2 1 5 4 2 2 2 3 4 1 3 2 3 4 5 2 1 3 3 3 3 4 2 4 0 18 2 4 4 3 4 3 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 3 2 3 4 3 4 4 4 3 4 1 4 1 3 2 3 3 2 1 0 0 1 0 3 1 1 0 0 0 1 0 0 0 "Art" 13 3 2 1 0 1 1 1 0 0 1 1 3 1 1 3 2 3 0 0 0 0 0 0 0 4 4 4 3 4 4 4 4 2 1 3 4 101 101 6 5 5 5 4 4 5 5 5 5 5 5 4 4 4 5 5 5 4 5 5 5 5 5 5 4 5 1 18 1 4 4 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 1 2 4 2 1 2 2 4 4 3 3 1 3 2 1 2 2 3 1 1 2 1 1 0 0 0 0 4 1 1 1 0 0 0 0 0 0 "Business Administration" 15 3 2 1 0 1 0 0 0 0 1 1 1 1 1 2 3 1 1 1 0 0 0 0 0 4 4 4 1 3 4 3 3 4 1 3 4 10 50 6 5 5 5 5 5 1 5 5 5 3 5 5 4 5 3 3 2 5 5 5 5 5 5 5 5 5 1 18 1 4 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 2 3 4 4 3 4 4 3 1 3 1 4 3 4 3 1 4 1 1 0 0 0 0 4 1 1 0 0 0 1 0 1 0 "Art" 17 3 2 1 0 1 1 1 0 0 1 1 3 101 101 101 3 3 1 1 0 0 1 0 0 4 3 4 3 3 3 4 3 4 2 3 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 4 4 4 3 3 3 4 4 4 4 4 4 3 1 18 2 4 3 3 3 4 3 3 1 1 1)

  ,(Ans 6 3 4 2 4 3 3 3 4 4 3 3 3 4 2 3 3 2 2 2 2 3 2 2 3 1 1 0 0 0 3 1 0 0 0 0 1 0 0 0 "Architecture" 17 3 2 1 0 0 1 0 0 0 1 2 3 1 1 1 3 1 0 0 0 0 0 0 0 3 1 1 1 2 3 2 1 1 1 2 3 10 60 6 3 2 1 0 3 1 5 2 3 2 1 3 2 1 4 3 3 4 3 3 3 3 3 3 3 3 0 18 1 3 2 3 3 2 2 3 3 2 3)

  ,(Ans 6 5 3 3 3 4 4 2 2 4 2 3 3 4 1 4 2 2 2 2 3 1 2 2 2 1 0 0 0 0 4 1 1 0 0 0 1 0 1 0 "Computer Science" 14 3 101 1 0 1 1 1 0 0 1 2 2 1 2 1 3 1 1 0 0 0 0 0 0 3 3 2 2 3 3 101 101 101 101 101 101 16 90 6 5 5 4 1 5 5 5 5 5 2 5 3 4 2 4 4 3 1 3 3 2 3 3 4 4 4 1 18 1 1 2 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 3 4 4 3 3 3 3 4 2 4 4 4 3 3 2 2 3 3 3 2 2 2 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Did not answer" 12 3 2 1 0 1 1 1 0 0 3 3 3 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 2 3 2 3 101 101 101 101 101 7 40 6 3 3 4 2 3 3 4 3 3 0 4 3 3 3 4 4 3 2 3 3 3 3 3 3 3 3 0 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 5 3 2 3 4 4 4 4 4 4 4 2 1 4 4 1 3 1 4 1 1 2 1 1 1 1 1 0 0 0 3 1 0 0 0 0 0 0 0 1 "Architecture" 13 3 2 1 0 1 1 1 0 0 1 1 101 3 101 101 3 1 1 0 0 1 1 0 0 101 101 101 101 101 101 3 3 3 4 3 2 15 45 6 5 5 5 4 5 5 4 5 5 5 5 3 3 2 5 5 4 3 4 5 5 3 4 4 4 4 1 18 1 2 2 3 3 3 3 2 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 2 4 4 3 2 3 4 4 4 4 3 3 3 2 1 0 0 0 0 4 1 1 2 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 0 0 0 0 0 1 1 3 1 1 1 3 1 1 0 0 0 1 0 0 2 2 1 2 3 3 101 101 101 101 101 101 15 35 6 4 4 2 2 5 4 5 5 5 4 5 5 5 5 5 4 5 4 5 4 4 4 4 4 4 4 1 18 1 2 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 3 3 3 3 2 2 3 3 4 2 4 3 2 2 1 2 1 2 2 1 2 4 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Electrical Engineering" 15 3 3 1 1 1 1 1 0 0 1 6 2 1 1 1 3 2 0 0 0 0 1 0 0 4 3 2 4 4 3 4 3 1 4 4 2 15 45 6 4 5 5 5 5 4 5 5 5 1 5 2 3 3 3 3 2 2 3 3 3 4 4 3 3 3 1 18 2 5 5 3 2 3 3 3 3 0 3)

  ,(Ans 6 5 4 3 4 4 4 3 2 4 4 4 4 3 3 1 2 1 2 1 1 1 1 3 1 0 0 1 0 0 5 1 1 0 0 0 1 0 0 0 "Business Administration" 15 3 99 1 0 0 0 0 0 0 1 1 3 1 1 2 2 3 1 1 0 0 1 0 0 4 4 4 4 4 4 3 3 3 3 4 3 101 101 6 5 5 5 5 5 4 5 2 5 4 5 5 5 4 5 5 3 5 3 5 4 5 4 4 5 5 1 18 1 4 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 4 1 1 4 4 1 4 4 4 4 4 4 4 1 3 4 2 4 4 1 3 1 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Civil Engineering" 15 3 2 1 0 1 1 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 1 4 3 4 3 2 1 4 3 101 101 5 5 5 4 0 4 3 3 3 2 2 3 4 3 101 101 2 3 2 3 3 4 3 4 4 4 4 1 18 1 4 5 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 3 2 4 3 2 1 4 3 3 4 1 2 2 4 3 1 2 1 1 2 2 2 4 0 0 0 0 1 4 1 1 7 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 1 1 0 0 0 1 3 1 1 2 1 2 1 1 0 0 0 0 0 0 3 4 2 2 3 3 2 2 1 1 2 3 101 15 6 5 5 4 2 5 5 4 2 4 2 4 3 3 3 4 4 4 3 2 3 3 3 3 3 3 3 1 18 2 4 3 3 3 2 3 2 3 0 3)

  ,(Ans 6 6 4 1 4 4 3 2 4 4 4 4 4 4 3 1 2 1 2 2 3 3 4 4 4 1 0 0 0 0 3 1 1 7 0 0 1 1 0 0 "Film" 16 3 2 1 0 1 1 0 0 0 1 6 3 2 1 1 2 3 1 1 0 0 0 0 1 4 2 3 2 3 3 3 1 3 1 1 2 101 101 6 2 3 2 1 5 3 3 1 2 2 1 3 3 2 4 4 5 3 2 3 1 4 4 3 4 4 1 18 2 2 2 3 3 4 3 2 1 1 1)

  ,(Ans 3 5 3 4 4 4 4 4 4 4 4 4 1 3 1 4 1 1 1 1 1 1 4 2 4 1 0 0 0 0 3 0 1 7 1 1 1 0 0 0 "Health Studies" 15 3 2 1 0 0 0 0 0 1 1 3 1 1 3 1 2 1 1 1 0 0 1 0 0 2 3 1 1 3 2 1 4 2 2 2 1 6 15 6 5 5 5 0 1 3 2 1 2 1 3 1 1 1 5 5 1 1 2 4 3 5 3 4 3 4 1 18 1 2 3 2 3 4 3 3 1 2 1)

  ,(Ans 5 5 1 2 4 4 4 1 3 4 3 4 4 4 2 4 4 3 4 4 3 2 2 3 1 1 0 0 0 0 1 0 0 99 0 0 0 0 0 0 "Undecided" 101 3 2 1 0 0 0 0 0 1 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 3 3 4 2 1 4 2 2 3 1 1 4 18 40 6 4 5 1 5 5 3 2 1 2 1 1 3 5 3 4 4 5 3 2 5 2 5 3 3 1 4 0 18 2 3 5 3 3 2 3 3 1 2 1)

  ,(Ans 6 3 4 1 4 4 4 4 2 1 2 3 4 4 4 4 4 2 4 4 4 3 1 4 3 1 0 0 0 0 4 1 1 7 0 1 1 0 0 0 "Business Administration" 17 3 2 1 0 1 0 1 0 0 1 6 3 1 1 1 3 1 1 1 0 0 1 0 0 4 4 4 3 4 4 4 2 4 3 1 4 15 25 6 5 3 4 2 5 5 5 2 5 2 5 4 4 5 4 3 3 4 2 4 3 5 5 5 5 5 1 18 2 3 4 3 4 2 3 3 1 0 1)

  ,(Ans 6 3 3 1 4 4 4 3 2 1 3 2 3 2 4 1 3 1 3 2 4 3 2 4 3 1 1 0 0 0 3 1 1 0 0 1 0 0 0 0 "Undecided" 13 3 2 1 0 1 0 0 0 0 1 3 3 1 1 3 2 2 1 1 0 0 0 0 0 4 3 2 2 4 2 3 3 3 2 3 2 0 5 6 4 5 2 4 4 5 5 3 2 5 3 2 3 1 3 4 2 3 2 1 4 3 4 3 4 4 1 18 2 4 4 3 3 3 3 3 1 0 1)

  ,(Ans 3 1 3 3 4 4 4 3 3 4 4 4 4 3 3 3 2 3 3 3 3 3 3 3 3 1 0 0 0 0 2 1 1 1 0 1 0 0 0 0 "Architecture" 15 3 1 1 0 0 0 0 0 0 2 1 3 1 1 1 1 2 1 0 0 0 0 0 0 4 4 3 3 3 3 3 4 3 3 3 3 1 10 6 5 4 2 2 5 5 4 4 4 3 5 2 2 3 2 3 4 3 2 2 2 2 4 3 3 3 1 18 1 4 4 2 2 2 2 2 0 0 0)

  ,(Ans 6 5 4 4 4 4 4 4 2 2 3 2 4 4 2 1 4 1 4 2 2 2 1 4 2 0 0 0 1 0 4 1 1 0 0 0 0 0 0 0 "Business Administration" 16 3 2 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 3 3 1 3 3 3 2 3 4 101 101 6 5 5 5 5 5 1 5 5 5 1 2 2 2 4 5 4 3 3 3 4 3 4 3 4 2 4 1 18 2 4 3 3 4 4 3 3 1 2 1)

  ,(Ans 6 6 4 2 2 3 4 4 4 2 3 1 4 4 4 4 3 2 3 3 3 2 2 3 3 1 0 0 0 0 4 1 1 4 0 0 0 0 0 1 "Business Administration" 13 3 2 1 0 0 1 0 0 0 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 4 4 4 3 3 4 4 4 4 3 3 4 20 30 6 5 5 5 2 3 4 5 3 4 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 2 5 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 3 1 4 3 3 2 4 4 1 4 1 4 1 1 2 2 1 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Administration of Justice" 17 3 2 1 0 1 0 0 0 0 1 6 1 1 1 1 3 1 0 0 0 0 0 0 0 4 4 4 3 4 4 3 3 4 3 3 3 22 80 6 3 5 4 5 4 2 2 0 3 0 3 3 2 3 2 2 2 2 1 4 2 2 4 4 3 2 1 18 2 99 3 3 4 3 3 3 1 2 1)

  ,(Ans 6 6 4 3 3 3 3 3 3 3 3 3 3 3 2 2 3 2 3 3 3 2 2 2 3 1 1 0 0 0 4 1 0 0 0 0 0 0 0 0 "Business Administration" 14 3 2 1 0 1 1 0 0 0 1 2 1 1 1 1 3 1 0 0 0 0 0 0 0 3 3 4 3 3 3 3 3 4 2 3 3 15 60 6 5 5 4 2 5 4 3 3 5 3 5 4 4 5 5 5 5 2 3 5 3 5 4 3 4 4 1 18 1 2 1 3 3 3 3 3 1 2 1)

  ,(Ans 3 5 3 1 2 4 4 2 2 2 4 3 4 3 3 1 3 1 2 2 4 3 1 4 3 1 0 0 0 0 3 0 1 0 0 1 1 0 0 0 "English" 13 3 2 1 0 0 0 0 0 0 1 6 3 1 1 2 1 3 1 1 0 0 0 1 0 4 4 4 2 2 2 4 4 4 2 2 2 101 101 6 3 2 2 4 4 1 2 1 1 1 1 2 2 1 4 4 1 2 1 2 2 1 4 4 5 5 1 18 2 2 3 3 4 3 3 3 1 2 1)

  ,(Ans 3 5 2 4 4 4 3 2 3 3 2 4 3 2 3 1 3 1 3 3 2 2 1 3 4 1 0 0 0 0 4 1 1 4 0 0 1 0 0 0 "Did not answer" 15 3 101 1 0 0 0 0 0 0 1 6 3 1 1 2 2 3 0 0 0 0 0 0 1 3 2 3 2 4 3 3 2 3 1 4 2 101 101 6 5 5 4 5 3 1 3 3 4 1 4 4 3 3 4 4 2 2 2 3 3 5 2 3 2 2 1 18 2 2 5 3 3 2 3 2 1 0 1)

  ,(Ans 6 3 4 4 4 4 4 4 2 4 3 2 2 4 2 1 3 1 4 3 3 1 2 3 4 1 0 0 0 0 2 1 1 0 0 0 0 0 0 1 "Business Administration" 16 3 99 1 0 0 0 0 0 0 1 3 1 2 2 1 3 1 1 0 0 0 0 0 0 4 4 4 3 4 4 2 3 3 1 4 2 9 35 6 5 5 5 5 4 5 5 3 4 5 5 2 3 4 4 4 4 5 2 3 5 3 4 4 4 4 1 18 2 1 1 3 4 4 3 3 0 0 0)

  ,(Ans 6 4 4 1 3 3 3 3 2 3 3 3 3 3 2 3 1 1 1 1 3 1 1 2 3 1 0 0 0 0 3 0 1 2 0 0 0 0 0 1 "Biology" 15 3 2 1 0 1 0 0 0 1 1 3 1 2 2 1 1 2 0 1 0 0 0 0 0 1 2 2 2 1 1 3 2 2 2 2 3 27 45 6 5 5 4 5 4 3 5 3 5 4 5 4 5 5 5 5 5 5 4 5 5 5 4 4 3 4 1 18 2 2 3 3 4 2 3 3 3 0 3)

  ,(Ans 5 6 101 1 3 3 2 2 3 2 4 2 3 3 2 3 2 1 3 2 3 2 3 3 2 1 0 0 0 0 2 1 1 99 0 0 0 0 0 1 "Business Administration" 13 3 2 1 0 1 0 0 0 0 1 2 1 3 1 2 2 1 0 0 0 0 0 0 0 3 2 3 3 3 3 2 2 2 2 3 3 6 28 6 5 5 4 4 5 4 3 4 4 3 2 3 4 4 3 4 4 3 4 4 4 3 3 3 1 3 1 18 1 5 5 3 2 1 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 3 3 4 4 4 4 3 4 4 4 3 4 2 2 2 1 3 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 12 3 2 1 0 1 0 1 0 0 1 2 1 3 1 1 2 1 1 0 0 0 0 0 0 4 3 4 3 4 4 3 2 3 2 3 3 15 30 6 2 5 4 5 4 4 5 4 5 3 4 3 3 3 3 3 3 3 3 3 2 4 3 3 2 2 1 18 1 4 3 3 3 2 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 2 4 1 4 4 4 3 3 2 1 2 3 2 2 2 1 4 1 1 1 0 0 0 0 3 0 1 7 0 0 0 0 0 0 "Biology" 12 3 101 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 2 3 4 4 101 4 2 3 4 101 101 6 4 5 5 1 2 3 2 2 2 5 5 4 4 4 3 4 4 3 2 2 3 3 2 4 1 1 1 18 1 2 1 3 3 2 3 3 0 0 0)

  ,(Ans 6 6 3 3 3 3 4 4 4 4 3 4 4 4 4 4 3 3 3 4 3 3 3 3 4 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Undecided" 12 3 2 1 0 0 0 0 0 0 3 1 3 1 1 1 3 1 1 1 0 0 0 0 0 3 3 3 4 4 4 101 101 101 101 101 101 12 45 5 3 3 5 4 4 5 5 5 5 5 5 5 5 4 4 5 4 4 4 4 3 4 4 4 4 4 1 18 1 1 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 4 4 4 4 4 4 3 2 4 2 4 4 3 3 1 3 1 3 1 2 1 3 3 3 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Business Administration" 15 3 99 1 0 1 1 1 0 0 1 2 3 1 2 2 2 3 1 0 1 0 0 0 1 4 3 4 2 4 4 4 3 3 1 4 4 101 101 6 5 2 4 4 4 3 4 2 2 5 4 4 4 3 5 5 4 4 4 5 5 4 3 4 4 4 1 18 2 2 2 3 3 2 3 3 0 0 1)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 3 4 4 3 4 3 3 4 4 4 4 4 4 1 0 1 0 0 3 1 1 7 1 1 1 0 1 0 "Undecided" 16 3 2 1 1 1 1 1 0 0 1 2 1 2 3 1 2 1 1 1 0 0 1 0 0 3 4 3 3 4 3 3 4 3 3 4 3 12 25 6 0 5 4 4 3 4 5 4 5 3 5 3 3 2 4 5 3 3 3 2 2 3 4 4 3 3 1 18 2 4 5 3 3 3 3 3 1 0 1)

  ,(Ans 3 6 3 1 4 4 4 2 1 3 4 4 4 3 3 1 2 3 3 1 1 1 1 3 1 0 0 0 1 0 3 0 1 3 0 1 0 0 0 0 "Art" 15 3 101 1 0 1 0 0 0 0 1 3 3 1 2 1 2 3 0 0 0 0 0 0 0 3 4 3 4 4 4 3 2 1 1 1 3 101 101 6 5 5 3 5 5 3 4 3 4 5 3 2 3 2 5 5 3 2 1 4 3 3 3 4 3 3 1 18 2 4 4 3 4 4 3 3 2 2 2)

  ,(Ans 6 6 3 3 1 3 3 2 2 2 3 3 4 2 2 3 2 1 2 1 2 1 2 2 2 1 0 0 0 0 2 1 1 7 0 1 1 0 0 0 "Art" 12 3 2 1 0 1 0 1 0 0 1 1 2 1 2 1 3 1 0 0 0 0 0 0 0 3 2 3 3 3 3 3 3 3 3 2 4 5 40 6 5 5 0 1 3 3 5 0 0 1 1 2 3 1 4 4 3 3 1 1 1 2 3 3 3 3 0 18 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 3 1 3 3 4 2 2 4 4 3 1 3 2 1 3 1 3 1 1 3 1 2 1 1 0 0 0 0 4 1 0 7 0 0 0 0 0 0 "Psychology" 15 3 2 1 0 0 0 0 0 0 101 1 3 2 2 1 3 1 1 0 0 1 0 0 0 4 4 4 3 4 4 3 3 3 3 4 2 15 40 6 5 5 5 5 5 5 3 4 4 2 5 3 3 4 4 4 4 3 4 4 2 4 3 3 3 3 1 18 2 3 3 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 2 4 4 4 3 3 4 4 3 4 3 3 4 3 101 3 3 3 3 3 3 3 1 0 0 0 0 2 1 0 7 1 0 1 0 0 0 "Business Administration" 13 3 2 1 0 0 0 0 0 0 1 2 1 3 1 1 1 1 0 0 0 0 0 0 0 4 4 3 4 4 3 101 101 101 101 101 101 20 25 6 4 2 2 4 2 2 3 2 2 4 1 3 3 2 3 3 4 4 3 3 3 4 3 3 3 4 1 18 2 1 2 2 2 2 2 3 0 0 0)

  ,(Ans 3 6 3 1 2 3 4 4 4 4 3 4 3 4 4 4 3 2 4 4 4 4 2 2 4 1 0 0 0 0 3 1 0 2 0 0 1 0 0 0 "Business Administration" 4 3 2 1 1 1 0 0 0 0 1 2 101 3 3 101 101 1 1 0 0 0 0 0 0 3 3 3 2 3 3 3 3 3 2 3 3 15 20 6 5 5 5 5 5 5 5 5 5 5 5 3 3 2 3 4 3 3 3 3 3 3 3 4 3 4 1 18 2 3 4 3 4 3 3 3 0 2 3)

  ,(Ans 6 5 4 4 4 4 4 4 3 4 4 4 4 3 4 1 1 1 1 1 1 1 1 1 2 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Psychology" 15 3 101 0 0 0 0 0 0 1 1 6 3 1 1 1 2 3 1 0 0 1 0 0 1 3 4 4 3 3 2 3 3 4 1 2 2 101 101 6 5 5 5 2 4 3 5 3 5 2 3 3 4 4 3 4 4 2 3 4 4 4 3 4 1 99 0 18 2 4 4 3 3 4 3 1 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 2 1 4 4 4 4 4 1 1 4 2 4 4 4 1 1 3 2 1 1 0 0 0 4 0 1 7 0 0 0 0 0 1 "Business Administration" 16 3 2 1 0 0 0 0 0 0 1 2 3 1 1 2 3 3 1 1 1 0 1 1 1 4 4 3 2 3 1 4 4 4 4 1 1 101 101 6 5 5 5 5 4 5 5 4 5 5 5 5 4 4 4 5 4 5 4 4 3 3 5 5 5 5 1 18 2 4 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 3 3 4 3 4 3 4 2 4 3 1 3 3 3 2 2 2 2 3 4 1 0 0 0 1 4 1 1 4 0 0 0 1 0 0 "Chemistry" 12 3 3 1 0 1 1 0 0 0 1 6 2 1 2 1 3 1 0 0 0 0 0 0 0 3 3 4 2 4 3 2 2 3 1 4 3 8 30 6 5 5 5 5 5 5 3 3 2 2 4 3 3 3 4 3 5 2 2 3 2 2 4 3 4 4 1 18 2 5 1 3 3 2 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 3 1 4 3 4 4 2 4 1 3 1 2 1 1 1 4 3 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Psychology" 13 3 101 1 0 0 0 0 0 0 1 6 3 1 1 1 2 3 0 0 1 0 0 0 0 4 3 4 1 3 3 3 3 3 1 3 3 101 101 6 5 4 5 3 4 0 4 0 5 5 5 3 5 4 4 3 2 3 1 5 5 5 4 3 2 4 1 18 2 4 3 3 3 4 3 3 3 3 3)

  ,(Ans 3 3 3 3 4 4 4 4 3 4 3 2 2 4 3 1 3 3 3 3 3 3 3 3 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 13 3 99 1 0 1 0 1 0 0 1 6 3 2 1 1 2 3 0 0 0 0 0 0 1 3 2 3 2 3 3 3 2 3 2 3 3 101 101 6 3 3 3 3 4 4 3 3 3 2 4 1 3 3 4 4 3 2 2 3 3 3 3 4 3 3 1 18 2 2 99 3 3 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 2 4 4 4 1 3 1 1 3 3 1 1 3 1 2 4 4 1 0 0 0 0 2 1 0 7 0 0 1 0 0 1 "Health Studies" 17 3 1 1 0 1 0 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 0 0 1 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 5 5 5 5 3 3 5 3 4 4 3 5 1 18 2 1 1 3 3 2 3 2 3 0 3)

  ,(Ans 99 6 3 2 4 4 4 2 3 4 4 4 3 3 4 1 3 3 3 1 2 2 1 2 2 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Film" 14 3 2 1 0 1 0 0 0 0 1 2 3 1 2 1 2 3 1 0 1 0 1 0 1 3 2 2 3 3 3 1 1 1 3 4 1 101 101 6 5 5 5 5 5 5 5 5 5 3 5 3 4 4 3 3 3 5 2 2 4 4 4 5 4 4 1 18 1 3 5 3 4 3 3 3 3 0 3)

  ,(Ans 6 4 4 1 4 4 3 3 3 3 3 2 4 3 2 1 2 1 3 3 3 1 2 4 2 1 0 0 0 0 4 0 0 7 0 1 1 0 1 0 "Child and Family Studies" 18 3 2 1 0 1 0 0 0 0 1 2 3 1 1 1 2 3 1 1 0 0 1 0 1 3 3 2 2 3 2 2 3 1 1 2 1 101 101 6 5 5 5 5 2 3 2 101 4 3 0 3 4 2 4 5 2 3 3 5 5 5 4 3 5 5 1 18 2 5 5 3 3 4 1 3 3 2 3)

  ,(Ans 6 6 4 2 4 4 3 2 3 3 4 3 4 3 3 1 3 1 3 2 3 2 1 4 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Business Administration" 16 3 2 1 0 1 0 0 0 1 1 6 3 1 1 1 1 3 1 1 0 0 0 1 0 4 4 4 3 4 4 2 2 2 1 2 2 101 101 5 5 5 5 5 3 4 4 4 5 5 5 5 5 3 4 4 5 4 4 4 5 5 4 4 4 4 1 18 1 4 3 3 4 4 3 3 3 2 3)

  ,(Ans 6 5 4 2 3 4 4 2 3 3 3 3 3 3 2 1 3 2 3 3 2 2 2 3 3 1 0 0 0 0 4 1 1 7 1 1 1 0 0 0 "Art" 15 3 99 1 0 1 1 1 0 0 1 6 3 101 101 2 3 3 0 0 0 0 0 1 0 3 2 3 3 4 3 4 3 2 2 4 2 101 101 6 4 5 5 2 5 4 5 3 5 4 5 2 4 4 4 5 5 4 4 4 3 4 4 4 3 3 1 18 2 99 99 3 3 4 3 3 3 2 3)

  ,(Ans 6 6 2 4 4 4 4 4 3 3 4 2 4 3 3 1 3 1 2 1 3 3 1 2 4 0 0 0 0 0 2 1 1 4 0 1 0 0 0 0 "English" 14 3 101 1 0 1 1 1 0 0 1 6 3 101 101 2 101 3 1 1 0 0 0 0 0 3 4 4 2 4 4 101 101 101 101 101 101 101 101 6 5 5 5 4 0 3 4 5 5 5 5 3 1 1 4 4 1 2 2 2 2 1 5 3 1 3 0 18 2 1 2 3 3 4 3 3 3 0 3)

  ,(Ans 6 4 3 3 4 4 4 4 3 4 4 4 4 3 3 1 2 1 4 2 3 1 4 2 2 1 0 0 0 0 4 1 1 7 0 0 1 1 0 0 "Health Studies" 19 3 2 1 0 1 1 0 0 0 1 2 3 2 1 2 1 3 1 1 0 0 1 0 0 101 101 101 101 101 101 4 4 4 4 3 3 101 101 6 5 5 5 5 5 5 5 5 5 3 5 5 5 4 5 5 4 5 3 5 3 4 4 4 3 4 1 18 2 3 3 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 4 1 3 4 4 2 4 2 3 4 3 4 2 4 2 1 3 1 2 2 1 2 1 0 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Music" 15 3 2 1 0 0 0 0 0 0 1 6 1 1 1 2 3 1 0 0 0 0 0 0 0 3 3 3 3 2 3 4 2 2 2 1 3 10 45 6 5 5 2 3 3 2 5 2 3 4 4 2 3 2 4 4 3 4 4 4 4 4 3 4 2 2 1 18 2 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 3 3 3 3 4 4 4 3 2 4 4 2 3 3 3 1 3 3 4 3 4 3 3 4 2 1 0 0 0 0 4 1 1 101 0 0 1 0 0 1 "Communication Studies" 12 3 2 1 0 1 1 0 0 0 1 1 3 1 1 2 2 3 1 1 0 0 1 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 4 5 4 2 4 5 4 4 4 5 5 4 3 3 5 5 4 5 4 4 5 3 5 5 4 4 1 18 2 4 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 3 1 3 4 3 1 1 3 4 3 1 2 1 1 2 2 3 1 4 1 2 4 1 1 0 0 0 0 4 1 1 7 0 0 0 0 1 0 "Undecided" 18 3 3 1 0 1 0 0 0 0 1 3 1 1 1 1 3 2 1 1 0 0 0 0 0 4 3 4 2 4 3 3 1 4 1 4 3 7 40 6 5 5 1 1 0 1 4 0 3 0 5 4 5 4 4 3 3 2 1 3 2 5 4 4 3 2 1 18 2 2 3 3 4 3 3 3 1 1 1)

  ,(Ans 6 6 4 1 3 4 4 3 2 3 4 3 4 4 2 1 3 1 3 3 3 2 3 3 3 1 0 0 0 0 4 1 0 7 0 0 0 1 0 0 "Foreign Languages" 15 3 2 1 0 1 1 1 0 0 1 3 3 1 1 1 2 2 1 0 0 0 0 1 0 3 3 3 3 4 4 3 2 2 1 3 4 1 7 6 3 3 5 5 5 2 3 2 2 1 5 3 4 4 5 5 2 4 3 4 3 5 4 5 3 4 1 18 2 2 1 3 4 3 3 3 1 2 1)

  ,(Ans 5 6 1 3 4 4 4 4 4 3 3 3 2 4 4 4 3 1 4 4 4 4 2 4 4 1 0 0 0 0 5 1 1 0 1 0 1 0 0 0 "Undecided" 13 3 2 1 0 0 0 0 0 0 1 1 1 3 2 1 1 1 0 0 0 0 1 0 0 4 3 3 3 3 4 4 2 2 3 2 3 7 15 6 2 5 5 5 5 5 3 3 5 5 5 3 3 4 4 4 5 2 2 3 4 4 5 5 5 5 1 18 2 3 3 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 4 1 3 4 4 4 2 3 2 2 4 3 2 1 1 1 1 2 3 3 1 2 1 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "Art" 13 3 2 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 3 2 3 3 3 3 2 1 2 2 2 2 101 101 6 3 5 5 0 0 2 3 0 5 0 3 3 3 4 2 2 2 4 3 2 3 3 3 4 99 99 0 18 2 2 2 3 3 2 2 3 1 2 1)

  ,(Ans 6 6 4 1 4 2 2 1 1 4 3 1 3 2 1 4 1 1 2 2 1 3 1 1 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Did not answer" 14 3 2 1 1 1 1 1 0 0 1 6 1 3 2 1 1 1 0 0 0 0 0 0 0 3 2 2 1 2 2 3 1 1 1 1 1 12 20 6 3 3 5 5 5 3 3 2 5 3 1 3 4 5 5 5 4 3 3 5 3 5 3 4 99 4 1 18 2 2 5 3 3 3 3 3 0 0 0)

  ,(Ans 6 6 3 3 4 4 4 2 1 3 3 4 3 3 2 3 4 1 4 3 4 3 3 4 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Environmental Studies" 13 3 2 1 0 1 1 0 0 0 1 1 3 1 2 2 3 1 1 1 0 0 0 1 0 4 3 2 4 4 4 3 2 1 3 4 4 10 40 6 5 5 3 5 5 3 5 4 5 3 5 2 3 3 5 5 4 4 2 2 4 3 4 5 5 5 1 18 2 2 4 2 3 2 3 3 1 0 1)

  ,(Ans 3 5 4 1 2 4 4 4 2 2 2 2 3 3 3 1 3 1 3 3 3 3 3 4 3 0 0 0 1 1 2 0 1 99 0 0 0 0 0 1 "Environmental Studies" 12 3 2 0 0 1 0 0 0 0 1 1 3 1 1 2 2 2 1 1 0 0 0 1 0 4 3 3 1 1 3 4 3 3 1 1 3 0 1 6 5 5 5 5 5 5 5 5 5 5 5 3 2 3 4 4 4 4 3 4 4 4 4 4 3 4 1 18 2 4 5 3 4 2 3 3 1 2 1)

  ,(Ans 6 2 4 1 4 4 4 4 1 2 1 4 3 4 2 1 4 2 4 2 2 1 1 4 4 1 1 0 0 0 4 1 0 7 0 0 1 0 0 0 "Theater Arts" 13 3 1 1 1 1 1 0 0 0 1 101 3 1 1 1 3 3 1 1 0 0 0 1 1 4 4 4 2 4 2 3 2 3 1 3 2 101 101 6 5 5 5 5 5 5 0 5 5 5 5 4 5 4 5 5 3 2 3 4 4 5 4 5 4 5 1 18 2 5 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 5 3 1 4 4 4 2 4 4 4 3 4 4 4 1 4 1 4 1 1 1 3 2 3 1 0 0 0 0 2 1 1 0 1 1 1 0 0 0 "Did not answer" 14 3 2 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 1 0 0 3 4 4 2 4 2 2 3 3 1 4 1 101 101 6 5 5 5 5 5 2 4 1 5 5 5 4 4 1 5 5 1 3 2 3 5 3 4 5 4 5 1 18 2 3 3 3 4 3 2 3 3 0 3)

  ,(Ans 6 2 1 1 1 4 3 4 2 4 4 4 4 4 4 1 3 2 3 3 2 2 3 4 4 0 0 0 1 0 3 0 1 7 1 0 0 0 0 1 "Theater Arts" 15 3 101 1 0 0 0 0 0 0 1 6 3 1 1 2 2 3 1 1 0 0 1 0 0 3 4 4 3 3 3 3 4 4 3 3 3 101 101 6 5 5 5 3 5 5 5 5 3 4 5 3 3 4 5 5 4 5 4 4 4 4 3 5 4 5 1 18 1 3 3 3 4 4 3 3 0 0 3)

  ,(Ans 5 3 3 1 3 4 4 4 3 4 3 2 3 3 3 3 2 1 2 1 1 3 3 2 2 0 1 0 0 0 4 1 1 0 0 0 1 0 0 0 "International Studies" 17 3 2 1 0 1 1 0 0 0 1 2 3 1 2 1 3 1 1 0 0 0 0 0 0 4 2 3 3 4 2 4 2 4 2 2 3 5 60 6 5 4 2 0 5 3 0 3 3 0 2 4 2 2 3 4 4 3 4 4 4 3 4 4 99 99 0 18 1 4 3 3 4 2 2 2 3 2 3)

  ,(Ans 3 5 4 1 4 4 4 4 2 4 3 3 3 2 1 4 3 1 2 1 2 2 1 4 3 1 0 0 0 0 3 0 1 0 1 1 1 0 0 0 "Undecided" 14 3 2 1 0 1 0 1 0 1 1 6 1 2 2 1 2 1 1 0 0 0 0 0 0 4 4 4 4 4 4 4 3 4 2 3 2 15 45 6 5 4 3 1 4 2 1 1 2 2 2 3 3 2 3 4 2 3 2 3 2 2 4 4 3 3 0 18 2 99 2 3 3 4 3 3 3 0 3)

  ,(Ans 3 2 3 1 4 4 4 4 3 4 3 3 3 2 1 4 2 1 2 1 1 2 1 3 3 1 0 0 0 0 3 1 0 2 1 0 0 0 0 0 "Undecided" 14 3 2 1 0 0 0 0 0 1 1 6 1 2 2 1 2 1 0 0 0 0 0 0 0 4 4 4 3 4 4 3 2 3 2 4 3 20 30 6 5 5 5 2 5 4 2 2 4 1 3 3 3 3 4 4 2 3 2 3 3 3 4 4 3 4 0 18 2 99 2 3 3 2 3 3 0 2 1)

  ,(Ans 6 5 4 1 4 4 4 2 4 4 4 3 4 3 3 4 3 3 3 3 3 4 4 2 3 1 0 0 0 0 3 0 0 0 0 0 1 0 0 0 "Mechanical Engineering" 12 3 2 1 0 1 1 0 0 0 1 6 101 3 101 101 101 1 0 0 0 0 1 0 0 4 4 4 3 4 4 3 4 4 2 4 1 10 20 6 3 5 2 5 5 5 5 1 5 5 5 2 3 2 5 5 5 3 3 4 3 3 3 4 2 3 1 18 1 5 5 3 4 3 3 3 1 1 1)

  ,(Ans 3 5 3 3 4 4 4 3 3 4 4 4 2 2 3 4 2 1 2 2 2 4 3 3 1 1 0 0 0 0 4 1 1 2 0 0 1 0 0 0 "Undecided" 13 3 2 1 0 0 0 0 0 0 1 1 1 1 1 2 3 1 0 0 0 0 1 0 0 4 4 4 3 3 4 4 3 4 2 3 4 8 60 6 5 4 5 5 5 5 4 5 5 3 5 4 3 5 5 5 4 4 4 4 5 4 5 5 5 5 1 18 1 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 3 3 3 4 3 3 2 2 1 3 3 3 3 2 2 4 2 1 2 2 2 2 2 3 3 1 0 0 0 0 4 1 1 7 0 0 1 1 0 0 "English" 15 3 2 1 0 1 1 1 0 0 1 2 1 3 2 1 2 1 1 0 0 0 0 0 0 4 4 2 1 3 4 2 3 2 1 2 2 6 25 6 5 4 3 2 2 3 5 5 5 2 5 2 2 1 3 2 3 2 4 3 2 101 4 3 3 4 0 18 2 3 3 3 3 3 2 3 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 2 1 4 4 4 3 3 3 1 3 3 3 3 3 3 4 4 4 1 1 0 0 0 5 1 1 7 0 0 0 0 0 1 "Communication Studies" 18 3 2 1 0 1 1 0 0 1 1 2 3 1 2 1 1 3 1 0 0 0 1 0 0 4 3 2 2 4 4 4 3 2 2 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 3 5 4 5 3 4 1 18 1 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 5 4 1 1 2 4 2 4 4 4 3 1 3 3 1 2 3 2 2 2 3 1 1 1 0 0 0 1 0 3 0 0 0 0 1 0 0 0 0 "Business Administration" 13 3 2 0 0 1 0 0 0 0 1 3 1 3 2 1 1 1 0 0 0 0 0 0 0 4 3 3 3 3 2 3 3 2 3 3 2 12 20 3 1 1 1 2 0 0 0 0 1 1 0 3 2 2 3 101 2 3 2 2 3 3 3 3 3 2 1 18 1 2 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 3 3 101 4 3 3 4 2 4 3 3 4 4 2 1 4 1 2 2 3 2 1 4 4 1 0 0 1 0 3 1 1 7 1 0 0 0 0 0 "Music" 12 3 2 1 0 1 1 0 0 0 1 6 1 1 2 2 3 2 0 0 0 0 0 0 0 4 3 2 2 3 3 3 2 2 2 3 3 7 15 6 4 5 4 2 4 3 3 1 5 3 5 1 1 1 3 3 1 1 1 3 3 2 3 4 2 2 1 18 1 2 2 3 3 2 3 2 3 3 3)

  ,(Ans 6 3 3 2 4 3 4 4 4 4 4 4 1 3 3 4 3 2 2 2 3 3 1 2 3 1 0 0 0 0 3 0 1 0 0 0 0 0 0 1 "Did not answer" 14 3 2 1 0 1 1 0 0 0 1 3 101 2 101 101 3 1 0 0 0 0 0 0 0 4 4 3 2 3 3 3 2 3 1 3 3 15 30 6 5 5 5 5 5 3 5 3 4 4 5 4 4 3 4 5 4 4 4 3 3 3 4 4 4 4 1 18 2 3 4 3 4 3 3 3 3 2 3)

  ,(Ans 6 6 3 3 3 4 4 4 2 3 4 3 4 4 4 4 3 1 4 2 4 3 1 4 2 1 0 0 0 0 4 1 1 0 1 1 0 1 0 0 "Business Administration" 15 3 2 1 0 1 0 1 0 0 1 1 1 1 2 1 3 1 1 0 0 1 0 0 0 4 4 4 4 4 4 3 3 3 3 3 4 6 15 6 5 5 5 4 5 4 5 2 4 4 4 3 3 2 4 4 3 2 3 3 3 4 4 4 4 4 1 18 2 4 5 3 4 3 2 3 3 2 3)

  ,(Ans 6 6 4 2 3 4 4 3 4 4 4 3 3 4 3 1 3 3 2 2 3 3 2 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Film" 13 3 1 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 101 101 6 5 5 5 5 5 5 5 3 5 4 5 4 4 5 5 4 3 5 3 4 4 4 5 5 3 4 1 18 1 5 99 3 3 3 3 3 3 2 3)

  ,(Ans 5 3 2 4 4 4 4 4 3 4 3 2 2 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 3 0 1 2 1 0 0 0 0 0 "Did not answer" 9 3 2 1 1 1 1 1 0 0 1 3 101 101 101 101 3 2 0 0 0 0 0 1 0 101 101 101 101 101 101 101 101 101 101 101 101 20 30 6 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 4 4 4 4 0 18 1 2 2 3 101 3 101 101 101 101 101)

  ,(Ans 6 2 3 2 4 3 3 4 3 4 3 4 4 3 3 4 2 2 2 2 3 3 1 3 3 1 0 0 0 0 2 0 1 7 0 0 0 0 1 0 "Biology" 18 3 2 1 0 0 0 0 0 0 1 3 1 3 1 1 1 1 0 1 0 0 0 0 0 101 101 101 101 101 101 3 2 1 1 1 3 10 30 6 5 5 5 4 3 3 4 3 4 4 3 2 2 2 3 3 2 2 2 2 3 3 3 3 4 4 1 18 2 3 4 2 4 3 3 3 3 0 3)

  ,(Ans 6 3 3 4 4 3 3 4 2 3 1 3 3 2 1 4 1 1 1 1 1 1 3 1 3 1 0 0 0 0 3 101 0 7 0 0 0 0 0 1 "Child and Family Studies" 15 3 2 1 0 1 1 1 0 0 1 6 1 2 3 1 1 1 0 0 0 0 0 0 0 4 2 3 4 4 2 3 2 3 2 2 2 20 35 6 4 3 5 1 4 3 1 1 3 3 2 3 2 3 4 4 2 1 1 4 2 3 3 101 99 99 0 18 2 3 2 3 3 3 3 3 0 0 1)

  ,(Ans 6 6 4 2 4 4 3 4 2 4 3 3 3 4 3 3 3 1 4 3 4 2 1 3 3 1 0 0 0 0 4 1 0 0 0 0 0 0 0 0 "Business Administration" 12 3 2 1 0 1 1 1 0 0 3 3 3 2 2 1 3 1 1 0 0 0 1 0 1 4 4 4 4 4 4 3 4 4 4 4 3 20 50 6 4 4 4 4 3 3 5 5 4 4 5 3 4 4 5 5 3 2 4 3 3 3 4 4 3 3 1 18 1 1 2 3 4 4 3 3 1 0 0)

  ,(Ans 5 6 2 3 4 4 4 4 4 4 4 4 3 3 3 4 4 3 3 3 3 3 3 3 3 1 0 0 0 0 4 0 0 0 0 0 0 0 0 1 "Undecided" 13 3 2 1 0 1 1 1 0 0 1 1 3 101 101 101 3 1 0 0 0 0 0 0 0 3 2 2 2 2 3 2 1 1 1 1 2 40 30 6 1 5 2 1 5 3 5 5 3 2 1 3 3 3 4 5 4 5 4 3 2 3 3 4 3 3 1 18 1 3 2 3 3 3 3 3 1 2 1)

  ,(Ans 6 3 4 3 4 3 2 4 2 2 2 4 4 3 2 4 3 3 4 2 3 3 3 3 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Child and Family Studies" 15 3 2 1 0 1 1 1 0 0 1 2 2 1 2 1 3 1 1 0 0 0 0 0 0 4 3 3 3 3 4 3 2 2 2 3 3 40 60 6 5 5 5 5 5 4 5 5 5 3 4 4 3 3 5 5 3 4 3 4 3 4 2 4 3 3 1 18 1 2 2 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 101 3 4 3 4 2 4 4 4 4 3 3 2 4 3 2 3 1 2 2 2 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 15 3 2 1 0 1 1 1 0 0 1 2 1 2 3 1 2 1 0 0 0 0 0 0 0 4 3 3 3 2 2 4 3 3 2 2 2 15 45 6 5 3 4 2 5 2 4 1 2 0 4 2 4 4 4 5 3 1 2 3 1 2 3 4 3 3 1 18 2 5 2 3 3 3 3 3 1 1 1)

  ,(Ans 6 5 4 1 4 4 3 3 2 4 3 2 3 3 3 4 3 1 2 2 2 3 2 2 3 0 0 0 0 1 3 1 1 7 0 1 0 0 0 0 "Music" 12 3 99 1 1 1 1 1 0 0 1 3 2 1 1 1 3 1 0 0 0 0 0 0 0 3 2 2 3 4 3 3 2 2 2 4 3 10 60 6 5 5 5 5 3 4 3 2 5 2 5 3 3 3 3 2 4 3 3 4 3 4 3 4 2 4 1 18 1 99 99 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 4 3 4 4 3 4 3 3 3 2 1 2 1 3 2 4 2 1 1 0 0 1 0 3 1 0 0 0 0 0 0 0 0 "Biology" 13 3 2 1 0 1 1 1 0 0 1 2 1 2 3 1 1 1 1 1 0 0 1 0 0 4 4 4 4 4 4 3 3 3 2 3 3 25 30 6 5 5 3 5 3 3 4 2 2 5 4 3 3 3 5 5 3 3 4 4 4 3 4 4 3 4 1 18 1 3 3 3 4 2 3 3 3 2 3)

  ,(Ans 6 5 4 4 4 4 4 3 4 4 3 3 4 4 4 4 3 3 3 3 3 3 3 3 2 1 0 0 0 0 3 0 1 0 0 1 1 1 0 0 "Business Administration" 11 3 2 1 1 1 1 1 0 0 1 1 101 3 101 101 101 1 1 0 0 0 0 0 0 1 2 1 1 1 1 4 3 4 4 4 4 20 45 6 5 5 5 3 5 5 5 5 5 3 5 3 4 2 3 4 3 2 2 3 3 3 4 4 4 4 1 18 2 2 3 3 3 3 3 3 0 0 3)

  ,(Ans 6 6 101 4 4 4 4 4 4 4 3 4 4 4 4 2 4 2 4 4 4 3 2 4 4 1 1 0 0 0 3 1 1 1 0 0 0 1 0 0 "Mechanical Engineering" 14 3 2 1 0 1 0 0 0 0 1 1 3 2 2 1 3 3 1 0 0 0 0 1 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 3 4 4 4 4 3 4 4 3 3 4 1 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 101 2 4 4 3 2 3 3 3 3 3 4 4 2 1 1 1 1 3 2 1 2 3 1 0 0 0 0 4 1 1 7 1 0 1 0 0 0 "Theater Arts" 13 3 1 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 1 0 0 0 0 0 0 4 3 2 2 3 3 4 2 2 1 2 3 101 101 6 3 3 2 3 5 2 2 2 2 2 5 2 2 2 3 3 4 1 3 3 3 3 4 2 4 4 1 18 1 2 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 4 4 4 4 4 3 4 4 3 1 3 2 3 4 4 1 3 4 4 1 0 0 0 0 4 1 1 7 0 1 0 0 0 0 "Did not answer" 13 3 1 1 1 1 1 0 0 0 1 6 3 1 2 2 2 3 0 1 1 0 1 0 1 4 4 3 4 4 101 4 3 3 4 4 4 101 101 6 5 5 3 5 5 5 3 3 5 5 5 3 4 3 5 5 5 3 3 3 3 4 5 5 5 5 1 18 2 99 3 3 3 3 3 3 3 2 3)

  ,(Ans 5 1 2 4 4 4 4 4 4 4 4 2 2 3 1 4 3 1 4 3 4 2 1 1 1 1 0 0 0 0 5 0 0 0 0 0 0 0 0 1 "Psychology" 17 3 2 0 0 0 0 0 0 1 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 4 3 3 3 2 2 101 101 101 101 101 101 10 15 6 3 3 3 3 3 3 1 3 3 2 2 2 2 2 3 3 2 1 1 2 2 2 3 4 3 3 0 18 2 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 3 6 3 3 3 4 4 2 3 4 2 4 1 3 3 4 2 2 3 3 3 1 2 3 4 0 1 0 0 0 2 1 1 7 0 0 1 0 0 0 "Health Studies" 14 3 2 1 0 0 0 0 0 0 1 3 3 1 1 1 3 1 0 1 0 0 0 0 0 1 1 1 2 1 2 101 101 101 101 101 101 20 90 6 2 4 1 5 4 3 2 3 1 2 3 2 2 101 1 2 2 2 2 2 2 2 4 4 3 3 1 18 2 2 2 1 1 2 2 2 3 0 3)

  ,(Ans 6 5 4 1 4 4 3 3 1 3 4 2 3 3 3 1 4 1 3 2 2 1 1 4 1 0 0 0 0 0 3 1 1 4 0 0 1 0 0 0 "International Studies" 14 3 99 1 0 0 0 0 0 0 1 1 3 1 1 3 1 3 1 1 0 0 1 0 0 4 2 3 3 4 3 3 1 3 3 4 3 101 101 4 5 5 5 5 5 5 5 4 5 5 5 4 5 4 5 5 3 4 3 4 4 4 4 5 4 4 1 18 2 2 2 3 4 4 3 3 0 0 3)

  ,(Ans 6 3 4 1 4 3 4 4 1 2 3 3 4 3 3 1 3 1 3 3 4 1 1 2 1 1 0 0 0 0 4 1 0 4 0 0 0 0 0 1 "Foreign Languages" 16 3 101 1 0 1 0 0 0 0 1 1 3 1 1 3 3 3 0 1 0 0 0 0 0 3 3 3 3 3 4 3 3 3 3 3 2 101 101 4 3 3 4 3 3 0 2 3 3 3 4 3 3 3 4 4 2 4 4 4 3 3 4 4 3 4 1 18 1 4 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 2 4 4 4 4 2 1 3 4 4 4 2 4 4 2 3 3 2 3 2 4 4 1 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "International Studies" 13 3 2 1 0 1 1 1 0 0 1 6 1 3 2 2 2 1 1 1 0 0 1 0 0 4 4 4 2 4 3 4 3 4 2 4 3 8 25 6 5 5 5 5 5 5 5 0 5 5 3 4 4 5 3 4 5 5 3 5 5 5 5 5 5 5 1 18 1 2 2 3 3 2 3 2 3 3 3)

  ,(Ans 6 6 4 1 4 4 3 2 4 4 4 3 3 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 0 2 1 101 4 0 0 1 0 0 0 "Undecided" 13 3 99 1 0 1 1 1 0 0 101 2 3 1 1 3 2 3 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 18 1 3 3 2 2 2 2 2 3 0 3)

  ,(Ans 6 5 3 4 4 4 4 4 4 4 4 3 4 3 3 4 3 2 3 3 3 4 3 4 4 1 1 0 0 0 2 1 1 7 1 0 1 1 0 0 "Social Science" 12 3 2 1 0 1 0 0 0 0 1 3 1 1 1 1 3 1 1 1 0 0 0 0 0 4 3 4 2 4 4 4 2 4 2 4 3 15 40 6 2 5 4 5 5 3 5 3 3 4 4 1 3 2 1 2 2 1 1 3 2 3 3 5 4 5 1 18 1 1 1 3 3 4 3 4 3 0 3)

  ,(Ans 6 6 4 2 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 5 1 1 0 1 0 1 0 0 0 "Business Administration" 14 3 2 1 0 1 1 1 0 0 1 2 1 3 1 1 1 1 1 1 0 0 0 0 0 3 4 3 2 4 4 4 4 3 2 3 3 10 20 6 5 5 5 5 5 5 5 5 5 3 3 4 5 3 4 4 4 4 4 4 4 3 3 3 3 3 1 18 1 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 3 5 3 1 4 4 4 3 3 4 4 2 4 2 1 1 3 1 2 2 3 2 1 3 3 1 0 1 0 0 4 1 1 0 1 1 1 1 0 0 "Sociology" 16 3 101 1 0 1 0 0 0 0 1 6 3 1 1 1 1 2 1 1 0 0 0 0 0 3 2 3 2 3 3 3 2 3 2 3 3 1 2 6 5 4 4 1 5 5 5 4 4 5 2 4 2 3 2 2 4 2 4 4 4 3 4 4 3 1 0 18 2 5 5 3 3 4 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 3 3 4 3 101 3 2 2 4 2 1 3 1 3 3 1 2 1 1 0 0 0 0 3 0 1 0 0 0 0 0 0 0 "Undecided" 16 3 2 1 0 1 1 0 0 0 1 1 101 101 101 101 3 1 1 1 0 0 1 0 0 4 3 3 2 3 3 4 3 3 2 101 3 3 30 6 5 5 3 5 5 3 3 2 4 5 5 3 3 3 4 4 3 3 3 3 3 3 4 4 4 3 1 18 2 5 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 5 3 1 4 4 4 4 3 3 4 2 4 3 1 1 2 1 2 1 2 1 1 2 1 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Film" 14 3 2 1 0 0 0 0 0 0 1 3 3 101 101 101 101 3 1 0 0 0 1 0 0 4 3 3 2 2 2 4 3 3 1 2 2 101 101 6 5 5 4 5 5 1 5 2 2 3 1 2 3 1 4 4 1 4 2 3 4 4 3 4 3 4 1 18 2 5 3 3 4 3 3 3 3 0 3)

  ,(Ans 6 5 3 2 3 4 4 3 2 4 2 4 3 3 3 4 3 2 3 3 2 3 1 1 4 1 0 0 1 0 4 1 1 0 0 1 1 0 0 0 "" 17 3 2 1 0 1 1 0 0 0 1 3 1 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 3 3 3 3 2 4 2 2 2 101 120 6 5 5 3 5 4 2 5 4 4 3 3 2 3 1 4 4 1 3 2 2 3 2 4 4 4 5 1 18 2 1 1 2 4 4 4 4 1 1 1)

  ,(Ans 6 3 4 4 4 3 3 3 4 4 2 1 4 3 1 4 2 1 3 2 1 1 3 1 1 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Business Administration" 15 3 99 1 0 1 1 1 0 0 1 1 1 3 3 1 1 1 0 0 0 0 0 0 0 3 1 4 3 3 4 3 1 4 2 3 1 12 20 6 5 5 5 1 5 5 5 5 5 5 2 3 3 3 3 3 4 2 3 3 3 4 3 3 3 3 0 18 1 2 2 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 101 4 4 3 101 2 3 4 3 4 3 3 3 2 2 1 2 2 2 2 2 3 4 1 0 0 0 0 3 1 1 0 1 0 0 1 0 0 "Business Administration" 15 3 2 1 0 1 1 0 0 0 1 6 2 3 1 1 3 1 1 0 0 0 1 0 0 3 4 3 2 3 2 3 3 3 2 2 2 50 45 6 3 2 2 2 3 2 5 0 1 2 1 1 1 2 3 2 4 3 1 1 1 2 4 4 3 4 1 18 2 1 1 3 4 1 2 2 0 2 1)

  ,(Ans 6 6 4 3 3 3 3 2 2 3 4 1 1 1 1 1 1 1 3 3 3 2 2 3 3 1 0 0 0 0 4 1 1 4 1 0 1 0 0 0 "Business Administration" 13 3 2 1 1 1 1 1 0 0 1 3 1 3 1 1 1 1 1 0 0 0 1 0 0 4 4 4 3 3 4 2 3 4 2 2 1 25 45 6 5 5 5 2 5 3 5 3 5 4 5 4 4 4 3 3 3 2 2 3 2 3 3 3 3 3 1 18 2 3 1 3 3 3 3 3 1 2 1)

  ,(Ans 6 3 4 2 4 4 4 3 4 4 3 4 4 3 3 4 3 1 3 1 3 1 1 3 3 1 0 0 0 0 3 0 1 7 0 1 0 0 0 0 "Child and Family Studies" 13 3 2 1 0 0 0 0 0 0 1 2 2 1 1 1 3 1 0 1 0 0 0 0 0 4 3 4 3 4 3 3 1 3 1 4 3 10 45 6 5 5 3 5 5 5 5 3 3 3 5 4 4 3 4 4 4 4 2 2 3 4 4 4 5 5 1 18 2 2 2 3 4 2 3 3 3 2 3)

  ,(Ans 6 4 4 1 3 4 4 4 2 2 1 4 3 4 3 3 1 1 3 3 4 1 1 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "International Studies" 13 3 101 1 0 1 0 0 0 0 1 6 1 1 3 1 2 1 0 1 0 0 0 0 0 4 4 4 4 4 4 3 3 3 1 4 1 101 30 6 5 5 5 5 5 5 5 5 5 5 5 2 4 4 4 4 1 1 2 2 4 4 5 4 4 3 1 18 2 1 5 3 4 4 4 2 3 2 3)

  ,(Ans 6 3 4 1 4 4 3 3 3 4 4 3 3 4 3 4 3 1 3 1 3 3 3 4 1 1 0 0 0 0 3 1 1 0 1 0 0 1 0 0 "Business Administration" 101 3 2 1 0 0 0 0 0 0 1 1 101 101 3 101 2 1 1 0 0 0 1 0 0 3 4 4 3 3 4 3 4 101 2 3 4 101 20 6 5 5 5 5 5 4 5 5 5 4 5 4 4 4 5 4 3 3 4 4 4 4 4 4 3 3 1 18 2 3 4 3 101 3 3 3 1 0 1)

  ,(Ans 6 5 3 4 4 4 4 2 3 4 2 3 3 3 2 1 3 1 3 3 2 3 1 3 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "International Studies" 15 3 101 1 0 1 1 1 0 0 1 3 3 101 101 101 101 3 0 0 0 0 1 0 1 4 4 4 4 4 4 3 3 3 3 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 3 4 3 3 1 18 2 2 4 3 3 4 3 3 3 0 3)

  ,(Ans 6 5 4 3 4 4 4 4 4 3 4 4 4 3 2 2 3 1 3 1 3 3 1 4 1 1 0 0 0 0 3 1 1 0 1 1 0 1 0 0 "Undecided" 12 3 2 1 0 1 1 0 0 0 1 1 3 1 1 2 1 3 0 0 0 0 0 0 1 3 3 3 3 1 3 3 3 3 2 1 2 101 101 6 3 3 5 5 4 1 5 3 3 2 3 2 2 2 4 4 2 3 2 2 2 2 3 3 3 3 1 18 2 4 2 3 3 3 2 3 3 0 3)

  ,(Ans 5 3 3 3 4 4 4 4 4 4 3 4 4 4 1 1 3 3 4 1 2 2 1 2 4 1 0 0 0 0 3 1 0 0 0 0 0 0 0 1 "Sociology" 16 3 99 1 0 1 0 0 0 0 1 3 3 1 1 2 2 3 1 1 0 0 1 0 0 3 3 4 2 3 4 3 2 3 1 3 3 101 101 6 5 5 5 5 5 5 5 3 5 1 5 3 4 3 5 4 3 4 3 3 4 5 3 4 5 5 1 18 2 5 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 3 2 3 3 4 3 2 1 1 3 1 3 1 3 1 1 3 3 1 0 0 0 0 5 1 1 4 0 0 0 0 0 0 "Business Administration" 16 3 1 1 0 1 1 1 0 0 1 4 1 1 2 2 3 2 1 1 0 0 1 0 0 4 3 4 4 4 4 4 2 3 3 4 3 5 15 6 5 5 3 5 5 5 5 1 3 3 5 4 4 3 4 3 5 4 5 5 5 4 5 5 4 5 1 18 2 4 3 3 4 4 3 3 3 3 3)

  ,(Ans 3 3 3 4 4 4 4 3 1 4 2 4 3 3 3 1 3 2 3 3 2 1 1 3 4 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Undecided" 15 3 1 1 0 1 1 0 0 0 1 6 3 1 1 2 2 3 1 0 0 0 0 1 0 4 3 4 4 3 3 3 2 4 2 3 2 101 101 6 5 3 4 5 5 2 4 3 2 4 4 4 2 2 4 5 2 4 2 3 3 2 4 4 4 4 1 18 2 99 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 4 3 3 4 4 4 4 4 4 4 2 3 2 3 3 3 4 1 4 4 1 0 0 0 0 4 1 1 0 1 1 1 0 0 0 "Undecided" 14 2 101 1 0 1 0 1 0 0 1 6 3 1 1 1 2 3 1 1 0 1 1 0 1 4 4 2 2 4 4 3 3 2 2 4 4 101 101 99 5 5 5 5 5 5 5 5 5 5 5 3 4 3 5 4 4 4 3 4 5 5 4 4 3 4 1 18 2 99 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 3 3 1 4 2 4 3 4 3 3 4 4 0 0 0 0 1 3 1 1 7 1 0 1 0 1 0 "Mechanical Engineering" 18 2 101 1 0 1 0 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 4 4 4 4 4 3 3 4 3 101 101 6 5 5 4 2 5 5 5 3 3 1 4 2 4 2 5 3 3 4 4 4 2 2 5 3 3 4 1 18 2 99 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 3 3 3 3 3 3 3 3 3 3 3 2 3 1 2 4 2 2 2 2 1 2 1 1 0 0 0 0 3 0 0 99 0 0 0 0 0 1 "Health Studies" 17 2 2 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 1 0 0 0 0 0 3 3 3 3 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 2 5 5 5 5 5 3 3 3 4 4 3 3 3 3 3 3 4 4 4 4 1 18 1 2 5 3 3 3 3 3 0 1 1)

  ,(Ans 6 5 4 2 4 4 3 2 1 3 3 3 4 3 2 1 3 1 4 2 3 2 1 2 2 1 1 0 0 0 4 1 0 7 0 0 0 1 0 0 "Undecided" 12 2 3 1 0 1 1 0 0 0 1 1 3 1 1 3 3 3 1 0 0 0 0 0 1 4 2 3 3 3 4 4 2 2 3 3 4 101 101 6 5 5 4 5 5 4 3 2 5 5 4 4 3 4 2 2 4 4 4 5 3 4 4 3 5 5 1 18 2 5 5 3 4 4 3 3 1 2 1)

  ,(Ans 6 4 4 3 3 4 4 3 2 2 3 3 2 2 3 4 2 1 2 1 2 1 1 1 1 1 0 0 0 0 4 1 0 2 0 0 1 0 0 0 "Mathematics" 17 2 2 1 0 0 0 0 0 1 1 2 3 1 2 1 3 1 0 0 0 0 0 0 0 4 3 2 2 3 3 3 3 2 1 3 3 30 65 6 5 5 5 2 4 5 2 2 3 4 2 3 2 4 5 5 5 2 5 4 3 4 4 4 3 3 0 18 1 3 3 3 4 4 2 3 3 2 3)

  ,(Ans 3 3 3 3 4 3 3 4 2 4 2 2 3 3 4 3 2 3 3 2 1 1 2 3 4 1 0 0 1 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 8 2 2 1 0 1 1 0 0 0 1 1 1 1 2 1 3 2 0 1 0 0 0 0 0 3 2 2 3 2 4 3 2 3 2 4 4 7 40 6 5 5 5 0 3 5 5 2 4 0 5 3 2 3 101 4 4 2 2 1 1 3 4 3 99 99 0 18 1 2 1 3 4 2 2 3 3 0 3)

  ,(Ans 6 5 3 101 4 4 4 3 4 4 3 4 4 4 4 4 3 1 3 1 2 3 2 4 2 1 0 0 0 0 3 1 1 4 0 0 0 0 0 1 "Art" 16 2 99 1 0 1 1 1 0 0 1 2 3 1 2 1 2 3 1 1 0 0 0 0 1 4 3 2 1 4 4 4 3 3 1 4 4 101 101 99 1 5 3 5 2 0 2 4 5 4 5 3 5 2 4 4 1 3 2 4 3 3 3 5 4 4 1 18 2 99 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 2 3 1 1 2 3 4 1 1 1 4 4 2 3 1 1 4 3 3 3 2 1 1 2 0 0 0 0 1 3 0 1 7 0 0 0 0 0 1 "Political Science" 101 2 2 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 1 0 0 1 0 0 101 101 4 101 101 4 4 3 101 2 3 4 101 101 6 5 5 3 1 3 5 5 4 5 4 5 3 3 2 4 4 3 3 3 2 2 3 3 4 3 3 1 18 1 3 4 3 3 4 3 101 0 0 0)

  ,(Ans 6 2 3 1 3 2 1 4 4 3 2 2 4 2 4 1 3 1 1 1 1 1 3 1 1 1 0 0 0 0 1 1 1 0 1 1 1 1 0 0 "Biology" 15 2 1 1 0 0 0 0 0 0 1 1 3 3 101 101 101 3 1 0 0 0 1 0 0 2 3 4 1 4 2 2 3 4 1 2 2 101 101 6 5 3 2 2 4 3 3 1 3 3 5 4 4 1 3 3 4 4 101 3 1 3 1 1 1 1 1 18 1 5 4 3 4 4 4 3 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 2 3 3 4 3 4 2 1 101 3 1 3 3 3 2 1 3 2 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "English" 16 2 101 1 0 0 1 1 0 0 1 3 3 1 1 1 2 3 1 1 0 0 0 0 0 4 3 3 2 3 3 3 2 2 1 1 1 101 101 6 5 4 5 5 4 4 5 2 5 3 4 4 5 4 3 3 4 4 3 5 3 4 4 4 3 4 1 18 2 4 4 3 4 4 3 2 3 3 3)

  ,(Ans 6 6 4 3 4 4 4 2 4 3 4 4 3 4 2 4 3 1 3 2 3 2 1 1 4 1 0 1 0 0 4 1 1 7 0 0 1 0 0 0 "Film" 15 2 1 1 1 1 0 0 0 0 1 6 1 2 1 1 3 1 1 0 0 0 0 0 0 4 3 3 2 3 4 3 2 3 2 3 3 30 80 6 5 5 5 3 3 3 4 3 5 2 5 4 4 3 4 4 3 4 3 4 4 4 4 4 5 4 1 18 1 4 3 3 4 3 3 3 3 3 3)

  ,(Ans 6 6 3 101 4 4 4 101 4 4 4 3 4 4 2 101 3 101 4 4 101 101 4 4 4 1 0 0 0 0 3 1 1 0 0 1 1 1 0 0 "Did not answer" 12 2 1 0 0 1 0 1 0 0 1 6 3 1 1 1 2 3 1 0 0 0 0 0 1 4 4 4 3 4 4 4 4 3 2 4 4 101 101 6 5 5 5 2 5 3 5 0 5 5 5 3 5 2 5 5 5 5 1 4 3 4 2 4 4 4 1 18 2 4 2 3 3 4 3 3 1 2 1)

  ,(Ans 99 3 4 1 1 4 4 4 1 1 1 3 3 3 3 3 1 1 1 1 1 2 1 1 1 1 0 0 0 0 3 0 1 99 0 0 0 0 0 0 "Mathematics" 18 2 2 1 1 1 1 1 0 0 1 2 3 1 1 1 3 1 1 0 0 0 0 0 0 4 4 3 4 3 4 4 1 1 4 1 1 3 35 6 0 5 5 5 5 5 5 0 5 5 0 1 5 5 5 2 3 5 1 5 5 5 3 5 3 5 1 18 1 5 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 3 4 4 4 4 2 3 3 4 3 4 3 2 2 3 3 3 3 2 2 2 2 1 0 0 0 0 2 1 1 2 1 0 0 0 0 0 "Undecided" 14 2 2 1 0 0 0 0 0 0 1 1 3 101 101 101 3 2 0 0 0 0 1 0 0 3 3 2 2 101 101 101 101 101 101 3 3 15 40 6 5 5 3 3 5 4 5 1 4 3 5 3 3 3 4 4 2 3 1 3 3 4 4 5 3 5 1 18 1 3 3 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 2 2 4 4 4 1 4 4 4 4 4 1 4 3 1 2 1 3 1 1 4 1 1 0 0 0 0 3 0 1 0 0 0 1 0 0 0 "Civil Engineering" 12 2 2 1 0 1 0 0 0 1 1 1 3 2 2 2 3 1 0 0 1 0 0 0 0 4 3 4 3 4 4 4 2 1 1 1 4 120 120 6 5 5 5 5 5 5 5 5 5 5 101 5 5 5 5 3 5 3 5 5 3 5 4 5 4 3 1 18 1 2 2 3 4 3 3 3 3 2 3)

  ,(Ans 6 6 4 2 4 4 4 3 3 3 3 2 2 3 3 1 3 3 3 3 3 3 3 2 1 1 0 0 0 0 2 1 1 0 0 1 1 0 0 1 "Did not answer" 13 2 99 1 0 0 0 0 0 0 1 1 3 2 2 2 3 99 1 0 1 0 0 1 0 4 2 3 2 2 3 4 2 3 2 2 2 101 60 6 5 5 4 4 1 5 5 0 5 5 5 1 1 1 1 1 3 2 2 2 2 2 4 4 4 4 1 18 2 3 3 3 3 4 3 3 0 0 0)

  ,(Ans 5 3 3 4 4 4 4 4 2 4 3 4 4 4 3 3 3 1 1 1 1 1 1 1 3 1 0 0 0 0 4 0 1 2 0 0 0 0 0 0 "Business Administration" 16 2 2 1 0 0 0 0 0 0 1 6 1 1 1 1 3 1 1 0 0 0 0 0 0 4 3 3 3 4 4 4 3 2 2 4 3 30 110 6 5 5 4 5 5 5 5 5 5 5 5 4 3 3 4 4 3 3 3 3 3 3 4 3 4 4 0 18 1 3 5 2 2 3 3 3 3 0 3)

  ,(Ans 3 6 4 1 3 4 4 2 2 2 4 4 4 4 2 1 3 2 3 3 3 2 2 3 2 1 1 0 0 0 5 1 1 0 0 0 0 0 0 0 "Political Science" 17 2 101 1 0 1 1 1 0 0 1 1 3 1 2 3 2 3 1 1 1 0 1 0 0 4 3 3 3 3 4 4 3 1 1 3 3 101 101 6 5 5 5 3 4 5 5 4 5 3 5 4 4 5 5 4 3 4 3 4 4 4 5 5 5 5 1 18 1 2 4 3 4 3 3 3 3 3 3)

  ,(Ans 6 5 1 1 4 3 4 4 1 2 4 3 2 2 1 2 3 1 3 1 2 2 1 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Undecided" 12 2 2 1 0 0 0 0 0 0 1 3 2 1 2 1 3 2 1 1 0 0 0 0 0 4 101 3 3 3 3 101 101 101 101 101 101 10 30 6 3 4 4 5 5 1 4 1 2 2 2 2 4 3 4 5 4 3 4 4 4 4 4 3 3 4 1 18 2 5 5 3 3 4 3 3 3 2 3)

  ,(Ans 6 2 4 1 4 4 4 4 2 3 4 3 4 2 1 1 2 1 3 2 3 1 1 3 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Political Science" 17 2 101 1 0 1 1 1 0 0 1 1 3 1 1 2 1 3 1 1 0 0 0 1 0 4 3 3 3 4 3 2 2 1 1 3 2 101 101 6 5 5 5 101 101 101 5 5 4 2 5 3 4 4 4 3 5 3 4 3 2 3 4 4 3 3 1 18 1 5 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 3 3 4 4 2 1 3 3 3 1 2 1 1 2 2 0 1 0 0 0 4 0 1 0 0 0 1 0 0 0 "Economics" 12 2 2 1 0 0 0 0 0 0 1 2 3 101 101 101 101 3 0 0 0 0 0 0 0 4 4 3 3 3 3 3 3 2 3 3 3 101 101 6 5 5 3 5 5 4 4 3 2 4 3 3 4 3 4 4 4 3 3 4 4 4 4 4 2 4 0 18 2 4 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 3 4 4 4 4 4 2 3 3 4 3 3 3 4 3 1 4 3 4 3 4 4 4 1 1 0 0 0 4 1 1 0 0 0 1 0 0 0 "Social Science" 14 2 2 1 0 1 1 0 0 0 1 3 101 101 101 101 3 1 0 1 0 0 0 0 0 4 4 3 4 3 3 3 3 3 3 2 2 25 75 6 5 5 2 2 5 5 3 4 4 2 1 3 3 1 3 3 2 2 2 3 2 3 4 4 5 5 1 18 2 4 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 1 3 4 2 3 4 4 4 2 3 1 1 1 1 2 1 1 1 1 0 0 0 0 4 1 0 4 0 0 1 0 0 0 "Foreign Languages" 13 2 2 1 1 1 1 0 0 0 1 1 3 1 1 1 1 3 1 0 1 0 1 0 0 4 3 1 2 3 3 4 3 1 2 2 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 4 4 4 4 3 3 3 3 4 4 5 5 1 18 1 2 3 3 4 2 3 2 1 2 1)

  ,(Ans 6 3 4 101 4 3 3 3 1 4 4 3 4 3 4 3 2 1 3 2 2 1 4 4 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Psychology" 14 2 1 1 0 1 0 0 0 0 1 6 3 1 2 1 2 3 0 0 0 0 0 0 1 4 3 3 3 4 3 4 3 3 3 4 3 101 101 6 2 5 4 3 5 4 5 3 4 3 5 2 2 2 3 4 3 2 2 3 3 3 3 4 2 4 1 18 2 2 2 3 4 3 3 3 1 1 1)

  ,(Ans 6 3 4 1 1 4 4 3 1 3 4 3 4 4 4 1 3 1 3 3 4 3 4 4 4 1 0 0 0 0 3 1 0 4 0 0 0 0 0 0 "Anthropology" 14 2 101 1 0 1 1 1 0 0 1 6 2 1 1 3 2 3 1 1 0 0 0 1 1 4 4 3 3 4 3 4 4 3 3 4 3 101 101 6 4 5 5 5 5 1 3 3 4 5 5 3 3 3 5 5 2 1 2 4 5 4 5 5 5 5 1 18 2 2 1 3 4 4 3 3 1 1 1)

  ,(Ans 6 6 4 4 4 4 3 4 2 4 3 4 4 4 1 1 3 1 3 2 2 3 1 1 4 1 1 0 0 0 4 1 0 0 0 0 0 0 0 1 "Anthropology" 13 2 2 1 0 1 1 1 0 0 1 3 3 1 1 2 2 2 1 0 0 0 0 0 0 4 4 4 3 3 4 3 2 1 1 3 2 0 5 6 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 3 4 5 5 5 5 4 4 3 4 1 18 2 4 3 3 4 3 3 3 1 1 1)

  ,(Ans 3 3 4 1 1 4 2 2 2 3 4 4 4 3 4 1 3 2 4 3 3 4 3 4 3 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Economics" 14 2 101 1 0 1 0 0 0 0 1 6 3 101 101 101 101 3 1 0 1 0 0 0 1 1 1 2 2 1 3 101 101 101 101 101 101 101 101 6 2 3 2 3 2 1 2 2 2 3 1 2 2 2 4 4 1 2 1 2 4 3 5 5 4 4 1 18 2 3 3 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 3 4 4 4 3 4 4 4 4 4 3 2 1 2 1 2 2 2 2 3 3 4 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "History" 15 2 101 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 3 4 3 2 2 3 3 3 2 2 2 3 101 101 6 5 5 4 5 4 5 5 2 5 0 3 4 3 4 4 4 5 5 2 4 3 4 4 4 3 4 0 18 2 2 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 2 2 4 4 1 4 4 3 1 3 1 4 4 3 1 1 4 3 1 0 0 0 0 4 1 1 7 0 1 0 0 0 0 "Business Administration" 13 2 1 1 1 1 1 1 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 3 4 3 2 3 4 101 101 101 101 101 101 101 101 6 5 4 5 3 3 0 4 0 4 5 0 3 3 3 5 5 1 4 1 2 3 3 3 5 99 99 0 18 2 2 2 3 4 2 2 3 3 2 3)

  ,(Ans 5 2 4 2 1 4 4 4 1 1 4 4 4 4 1 3 2 1 4 1 4 2 1 4 4 1 1 1 1 0 2 1 1 7 1 0 1 1 0 0 "Chemistry" 15 2 2 1 0 1 1 1 0 0 1 6 1 1 2 2 3 1 1 1 0 0 0 1 0 3 4 3 4 3 3 4 4 2 4 3 4 10 40 6 4 5 5 5 5 1 3 4 5 5 5 1 2 5 5 5 3 3 2 3 3 5 5 4 4 5 1 18 2 1 3 3 4 3 3 3 3 3 3)

  ,(Ans 6 5 4 1 4 4 4 2 1 4 3 4 4 3 1 2 2 1 2 2 1 1 2 3 4 1 0 0 0 0 3 0 1 7 0 0 0 0 0 0 "Undecided" 15 2 99 1 0 1 0 1 0 0 1 3 1 3 2 1 2 2 1 1 0 0 0 0 0 4 3 3 2 4 4 4 3 1 1 4 4 15 45 6 5 5 5 5 5 4 5 5 5 5 3 4 4 4 5 2 3 3 5 4 5 5 4 5 5 5 1 18 2 2 1 3 3 4 3 3 3 2 3)

  ,(Ans 6 4 4 4 4 4 4 4 1 4 4 4 4 4 3 1 2 1 2 1 4 1 1 4 3 1 0 0 0 0 3 1 1 4 0 0 1 0 0 0 "Women's Studies" 15 2 1 1 1 0 0 0 0 0 1 6 3 1 1 3 3 3 1 1 0 0 1 0 0 4 4 3 3 4 4 4 4 2 2 4 4 101 101 6 5 5 5 5 5 5 5 4 5 5 5 2 2 2 5 5 1 5 2 3 4 3 5 5 5 5 1 18 2 5 4 3 4 3 3 3 3 2 3)

  ,(Ans 6 5 4 4 4 4 4 4 4 4 4 4 3 4 4 3 4 1 4 2 4 4 2 2 4 1 0 0 0 1 4 1 1 0 0 1 1 0 0 0 "Health Studies" 12 2 3 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 1 0 0 1 0 0 4 4 3 4 4 3 4 4 2 4 3 4 101 101 6 5 5 5 5 5 5 5 4 5 5 5 3 4 3 3 4 3 3 2 3 3 4 5 4 5 5 1 18 2 5 3 3 4 4 3 3 0 2 3)

  ,(Ans 6 2 4 4 4 4 4 4 3 4 3 4 2 4 4 2 101 2 4 4 4 3 2 3 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "" 13 2 2 1 0 1 1 1 0 0 1 1 3 1 2 2 2 3 1 0 0 0 0 1 1 4 4 4 4 4 4 4 4 4 3 3 3 101 101 6 5 5 5 5 5 5 4 5 5 5 5 2 2 3 3 5 5 2 3 4 3 3 4 5 5 5 1 18 2 3 3 3 3 3 3 3 0 0 3)

  ,(Ans 6 4 4 1 3 4 4 4 2 3 4 3 3 3 3 1 3 2 3 3 4 2 101 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Did not answer" 14 2 2 0 0 1 1 1 0 0 1 6 3 1 1 2 3 2 1 1 0 1 1 1 0 4 4 4 3 3 4 3 4 4 2 3 4 2 15 6 3 3 4 5 5 4 5 2 4 5 5 3 4 3 4 4 3 5 3 3 4 4 4 5 5 5 1 18 2 3 1 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 4 1 3 3 4 2 4 4 4 4 3 4 2 4 3 2 3 3 4 4 1 4 4 1 0 0 0 0 4 1 1 0 1 0 1 0 0 0 "International Studies" 14 2 2 1 0 1 1 0 0 0 1 1 1 3 3 2 2 1 1 1 0 0 0 0 0 4 4 4 4 4 4 4 3 3 3 4 2 17 45 6 3 3 5 5 5 5 5 5 5 5 2 1 2 4 4 4 4 3 2 2 1 1 5 5 5 5 1 18 1 2 2 3 2 2 3 2 0 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 2 3 3 3 1 2 3 1 3 2 3 3 2 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "English" 12 2 2 1 0 1 0 0 0 0 1 3 2 1 1 1 3 2 0 1 0 0 0 0 0 4 3 4 2 3 4 4 3 3 2 3 4 10 45 6 5 5 5 2 4 2 4 2 5 4 5 4 4 4 3 3 3 2 3 4 4 4 3 4 99 99 0 18 1 2 2 3 3 2 3 3 3 2 3)

  ,(Ans 6 5 4 1 4 3 4 3 2 4 4 2 2 4 3 3 3 2 4 3 3 1 2 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Biology" 12 2 2 1 1 1 1 1 0 0 1 6 3 3 1 2 3 1 1 0 0 0 1 0 0 4 4 4 3 4 4 3 2 4 2 3 4 10 40 6 5 5 5 5 5 5 5 2 5 5 5 3 4 4 5 5 4 2 2 3 3 4 4 4 3 3 1 18 2 3 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 2 3 2 2 1 2 3 2 2 3 4 4 3 2 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 18 2 2 1 1 1 1 1 0 0 1 2 3 1 1 2 2 3 1 1 0 0 0 0 0 4 3 2 2 3 4 4 3 2 2 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 4 3 3 4 5 5 4 5 4 4 4 4 5 5 4 5 1 18 2 4 4 3 3 3 3 3 3 3 3)

  ,(Ans 6 3 3 3 4 3 4 3 3 3 4 3 3 4 4 1 3 2 4 4 4 2 3 4 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Health Studies" 13 2 3 1 0 0 0 0 0 0 1 2 3 1 1 2 1 3 1 1 1 0 0 0 1 4 3 3 3 3 3 3 3 2 3 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 3 3 3 3 5 5 4 4 3 3 5 3 4 4 4 5 1 18 2 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 3 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Undecided" 13 2 2 1 0 1 1 1 0 0 101 1 3 1 1 3 2 3 1 0 0 1 1 1 0 4 3 4 3 101 3 3 3 3 3 3 3 101 101 6 1 1 1 2 5 5 5 3 3 3 3 1 1 1 1 5 3 4 3 3 3 3 5 5 4 5 1 18 1 4 5 3 4 4 3 3 3 3 3)

  ,(Ans 3 5 2 1 2 4 4 2 2 3 4 1 3 2 3 1 2 1 2 1 2 1 1 3 1 1 0 0 0 0 101 0 1 0 0 0 0 0 0 0 "Geology" 13 2 2 1 0 1 1 1 0 1 1 1 1 2 1 2 3 2 1 0 0 0 1 1 0 3 2 3 2 2 3 3 2 3 2 2 3 5 15 6 5 5 5 5 5 5 5 3 5 4 5 4 4 3 3 3 4 4 3 4 4 4 3 4 99 99 1 18 1 4 4 3 3 2 3 3 3 3 3)

  ,(Ans 6 3 4 1 3 4 4 4 3 3 4 3 4 4 4 1 3 1 3 4 4 1 1 4 3 1 1 1 0 0 3 1 1 7 0 0 1 0 0 0 "Women's Studies" 12 2 1 1 0 0 0 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 1 1 1 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 5 5 5 5 2 3 3 4 3 3 5 3 4 3 4 4 2 3 2 3 3 3 4 4 4 4 0 18 2 4 4 3 4 3 3 3 3 0 3)

  ,(Ans 6 4 4 4 4 4 4 4 3 3 4 3 4 4 2 1 3 1 4 3 3 3 3 2 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 0 "Psychology" 15 2 2 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 1 1 0 0 1 1 1 4 3 3 1 4 4 4 2 2 1 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 3 3 4 5 5 5 5 1 18 2 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 4 4 1 4 4 4 4 4 4 4 3 4 4 3 3 4 1 4 4 3 4 2 4 2 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Biology" 14 2 99 1 0 0 0 0 0 0 1 6 1 3 1 1 2 1 0 1 0 0 0 0 0 4 4 4 3 4 4 4 1 2 3 3 4 5 10 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 5 5 5 5 5 1 18 2 4 3 3 3 4 3 3 3 3 3)

  ,(Ans 6 5 1 1 4 4 4 4 4 4 4 4 4 4 4 2 4 101 4 4 4 2 1 4 4 1 1 0 0 0 3 1 1 1 0 0 1 0 0 0 "Did not answer" 14 2 1 1 1 1 1 1 0 0 1 2 2 2 2 1 1 3 1 1 0 0 1 1 1 4 3 2 3 3 2 4 3 2 1 3 1 101 101 6 5 5 5 5 5 5 5 4 5 5 5 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 1 18 2 2 3 3 3 3 3 3 0 0 3)

  ,(Ans 6 2 4 1 2 4 4 4 2 2 3 4 4 4 3 1 3 1 4 3 4 1 1 4 3 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Chemistry" 16 2 2 1 0 0 1 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 0 0 0 4 4 3 3 3 4 4 3 3 2 3 4 101 101 6 5 5 5 5 5 3 5 0 5 5 5 3 2 2 2 3 1 3 1 2 3 3 5 3 99 99 1 18 1 2 3 3 4 4 2 4 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 2 4 3 4 4 4 1 1 4 4 1 0 0 0 0 3 1 1 7 0 0 1 1 0 0 "Business Administration" 13 2 1 1 0 1 0 0 0 0 1 3 2 101 2 101 101 3 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 101 101 6 5 5 2 4 4 4 4 4 4 1 5 4 4 4 3 3 3 4 3 3 3 3 4 4 4 4 1 18 1 1 2 3 3 1 2 2 3 0 3)

  ,(Ans 6 6 4 4 4 3 4 3 2 4 4 3 4 4 2 1 3 3 4 2 4 1 1 4 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Business Administration" 15 2 2 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 1 1 0 0 0 0 0 4 4 4 4 4 4 4 3 3 2 4 2 101 101 6 5 5 5 5 5 4 4 4 5 5 5 3 4 3 4 4 4 2 4 5 4 4 4 101 4 5 1 18 2 3 2 3 4 4 3 3 0 0 0)

  ,(Ans 6 6 4 1 4 4 4 1 4 4 2 3 4 2 2 1 3 1 2 1 1 1 3 3 4 1 0 0 0 0 4 1 1 0 0 1 0 1 0 0 "English" 14 2 1 1 0 1 1 0 0 1 1 6 3 1 1 1 2 3 1 1 0 0 0 0 0 3 2 3 2 4 3 4 3 3 2 4 4 101 101 6 3 5 5 4 5 4 4 4 5 3 5 3 3 4 4 4 2 2 2 4 3 3 4 4 3 3 1 18 2 2 4 3 4 4 3 3 3 3 3)

  ,(Ans 4 6 2 1 4 1 2 1 4 4 4 4 1 1 1 4 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "Engineering" 13 2 2 1 0 1 1 1 0 0 1 6 3 2 1 1 2 3 1 0 0 0 0 0 0 4 4 1 1 4 1 3 2 1 1 2 1 101 101 6 5 5 4 4 3 4 5 5 5 4 4 4 4 4 3 4 3 4 4 5 5 3 4 4 3 3 1 18 1 2 4 1 1 1 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 3 3 1 4 3 3 4 2 3 1 3 3 2 1 1 1 1 3 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "International Studies" 14 2 101 1 0 1 0 1 0 0 1 6 3 101 101 2 2 3 0 1 0 0 1 0 0 3 4 3 2 3 3 3 4 2 2 3 2 101 101 6 5 5 5 3 4 4 4 3 5 4 4 4 4 3 5 4 4 4 4 4 3 4 4 3 4 3 1 18 2 3 4 3 4 2 3 3 3 0 3)

  ,(Ans 6 6 3 4 4 4 4 3 2 3 2 3 3 3 3 1 2 3 2 2 2 2 1 2 3 1 0 0 0 0 3 0 0 7 0 0 0 0 0 1 "Child and Family Studies" 12 2 101 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 0 0 0 0 1 0 0 4 3 3 3 4 4 3 3 2 3 4 4 101 101 6 5 5 5 5 4 5 5 5 5 5 5 3 3 2 3 3 4 3 2 3 4 4 4 4 4 5 1 18 2 2 4 3 3 4 3 3 3 2 3)

  ,(Ans 3 6 3 1 3 4 3 3 1 2 4 2 4 2 3 1 1 1 1 1 1 3 1 4 2 0 0 0 1 0 3 0 0 0 1 1 1 0 0 0 "English" 14 2 101 0 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 0 0 3 3 1 2 4 4 2 2 1 1 3 3 101 101 6 5 101 3 5 5 5 5 5 5 0 5 3 4 2 4 4 3 4 4 4 1 3 4 3 2 99 0 18 1 5 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 5 4 2 4 4 3 3 3 4 3 4 4 3 3 1 2 1 3 2 4 2 1 4 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 14 2 101 1 0 1 0 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 0 1 0 4 3 3 1 4 3 3 2 3 1 4 3 101 101 6 5 5 5 2 5 2 3 2 5 3 2 3 5 4 4 5 2 2 2 4 4 3 5 5 4 3 0 18 2 5 2 3 4 2 3 1 3 2 3)

  ,(Ans 6 3 4 1 4 4 2 3 1 1 4 4 4 4 3 1 3 1 3 3 3 4 1 3 2 1 0 0 1 0 4 1 1 7 0 0 0 0 0 0 "Music" 15 2 1 1 1 1 1 1 0 0 1 4 3 1 1 1 1 3 0 0 0 0 1 0 1 4 4 4 4 4 4 1 1 1 1 1 1 101 101 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 1 18 1 5 99 3 3 3 3 3 2 2 2)

  ,(Ans 6 4 4 1 3 4 3 4 2 3 4 4 3 4 3 4 3 1 1 1 2 1 1 2 2 1 0 1 0 0 4 1 1 0 1 0 1 0 0 0 "Biology" 15 2 101 1 0 0 1 0 0 0 1 2 1 2 1 1 3 1 1 1 0 0 0 0 0 3 3 3 4 3 3 3 2 2 4 2 2 101 50 6 5 2 5 1 1 2 2 2 0 0 2 2 3 4 3 4 2 3 3 2 2 3 4 4 4 2 1 18 2 2 2 3 4 4 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 4 4 1 4 3 3 4 4 2 1 3 1 4 4 4 1 1 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 14 2 1 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 1 0 0 0 1 0 1 4 2 3 3 3 3 3 2 2 1 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 3 5 4 4 4 3 4 4 4 5 5 5 5 1 18 2 2 2 3 3 2 3 3 3 2 3)

  ,(Ans 6 3 4 1 3 4 4 4 2 3 3 4 1 3 3 1 3 2 3 2 3 1 1 1 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Elementary Education" 15 2 1 1 0 0 0 0 0 0 1 1 3 1 1 2 1 3 1 0 0 1 1 0 0 4 4 3 2 4 2 4 4 1 1 4 2 101 101 6 2 2 3 5 5 1 5 2 3 5 3 1 1 3 5 5 3 5 3 4 5 5 3 5 3 3 1 18 2 2 2 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 2 3 4 2 1 4 1 4 2 3 3 3 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Elementary Education" 18 2 101 1 0 0 0 0 0 0 1 3 3 1 1 2 1 3 1 1 0 0 1 1 0 4 4 4 4 4 4 3 3 3 3 4 4 101 101 6 5 5 4 5 5 5 5 5 5 5 5 4 3 2 5 4 5 3 2 3 3 4 5 5 4 5 1 18 2 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 4 2 4 4 4 2 2 1 4 3 4 2 3 1 3 1 3 1 1 1 3 4 4 1 0 0 0 0 4 0 1 4 0 0 0 0 0 0 "Anthropology" 15 2 101 1 0 1 0 1 0 0 1 3 2 1 1 2 3 2 0 0 0 0 0 1 0 2 3 2 2 4 101 101 3 101 101 101 4 4 30 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 4 4 4 3 3 4 4 5 3 5 4 5 1 18 2 3 2 3 2 3 3 3 3 2 3)

  ,(Ans 3 5 2 1 4 4 4 3 4 4 4 2 3 3 3 3 3 1 4 2 2 3 1 3 1 1 0 0 0 0 3 1 1 4 0 0 0 0 0 1 "Art" 14 2 2 1 0 0 0 0 0 0 1 1 1 1 1 1 3 1 0 0 0 0 0 1 0 3 3 3 3 3 3 101 101 101 101 101 101 6 30 6 5 5 5 0 5 1 5 4 5 5 5 3 4 3 4 2 2 2 2 4 3 4 4 4 3 3 0 18 1 2 5 3 4 4 3 3 3 2 3)

  ,(Ans 3 5 4 1 2 101 4 1 3 3 3 4 2 2 3 1 3 1 3 2 2 1 1 3 4 1 0 1 0 0 3 1 1 0 0 0 1 0 0 0 "Art" 13 2 1 1 0 1 1 1 0 0 1 6 3 101 101 2 2 3 1 0 0 0 0 0 0 3 3 2 2 3 2 3 2 2 2 2 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 2 2 2 5 5 3 3 3 4 3 4 3 4 3 2 1 18 2 4 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 3 101 101 4 3 101 3 3 4 4 4 3 3 4 101 101 3 4 4 3 4 101 4 1 1 0 0 0 2 0 101 7 1 1 1 1 0 1 "Undecided" 12 2 1 0 0 1 1 0 0 0 1 3 1 1 101 2 3 1 1 0 0 0 0 1 0 3 4 3 101 101 101 3 4 3 101 101 101 4 50 6 3 2 101 4 0 101 3 101 101 101 101 1 1 1 1 1 1 1 1 1 1 1 5 3 4 4 1 18 2 2 2 2 4 4 2 1 0 0 0)

  ,(Ans 6 2 4 3 4 4 4 4 3 4 4 3 4 4 4 4 4 3 4 4 4 4 2 4 4 1 1 0 0 0 4 0 1 0 0 0 1 0 0 0 "" 12 2 99 1 0 1 1 1 0 0 1 1 1 3 1 1 1 1 1 1 1 0 0 0 0 4 4 3 2 4 3 4 3 3 101 4 3 101 25 6 2 3 3 5 3 2 5 3 3 2 5 3 5 3 3 5 3 3 3 3 3 3 4 5 5 5 1 18 2 99 99 3 3 4 3 3 1 2 1)

  ,(Ans 6 3 4 3 4 4 4 4 3 3 3 3 4 4 3 2 3 1 3 3 3 2 1 3 3 1 0 0 0 0 2 1 1 0 0 1 1 1 0 0 "" 101 2 99 1 0 1 0 0 0 0 1 6 3 2 2 2 2 3 1 1 0 0 0 0 1 4 4 3 3 3 4 3 3 2 2 2 3 101 101 6 4 5 5 2 3 2 1 1 1 1 3 3 3 3 4 4 2 2 2 3 3 3 4 4 4 4 1 18 2 4 4 3 4 4 3 3 1 1 1)

  ,(Ans 6 2 4 2 2 4 3 4 3 4 3 4 4 4 2 4 4 1 4 3 3 3 3 3 3 1 0 1 0 0 3 1 1 0 0 1 0 0 0 0 "Science" 18 2 2 1 0 1 0 0 0 0 1 6 1 3 2 1 2 1 1 1 0 0 0 0 0 4 3 3 3 2 4 4 3 3 101 2 3 14 20 6 5 5 4 4 5 4 4 3 5 5 5 4 4 4 4 4 4 5 3 4 4 3 5 4 3 101 1 18 2 4 2 3 4 3 3 3 1 0 1)

  ,(Ans 6 6 4 1 4 4 3 2 1 2 4 3 4 4 3 1 2 2 3 3 3 1 1 4 1 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Business Administration" 15 2 2 1 0 1 0 1 0 0 1 1 3 1 1 1 2 3 1 1 1 0 1 1 0 3 4 3 3 3 4 2 3 2 1 2 3 101 101 6 4 5 5 4 3 3 5 3 5 4 4 2 4 3 5 5 4 4 2 4 4 4 1 1 1 1 1 18 2 4 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 4 3 1 3 4 4 4 1 1 4 4 4 4 4 1 4 1 4 3 4 1 1 4 1 1 0 0 0 1 4 1 1 0 0 0 1 0 0 0 "Foreign Languages" 14 2 101 1 0 1 0 1 0 0 1 2 3 1 1 1 2 3 1 0 0 0 1 0 1 4 4 3 4 3 3 3 2 2 3 2 3 101 101 6 4 4 4 2 4 3 4 3 2 3 2 2 4 4 5 5 5 5 3 4 3 3 4 4 3 3 1 18 1 4 3 3 3 4 3 3 1 1 1)

  ,(Ans 6 5 4 1 4 3 3 3 2 3 4 3 4 4 3 1 3 3 3 3 3 3 2 101 3 1 0 0 1 0 4 1 1 0 0 0 0 0 0 0 "Computer Science" 12 2 101 1 0 0 0 0 0 0 1 2 3 1 1 3 1 3 0 0 0 0 0 0 0 4 3 3 2 3 3 3 3 2 2 3 3 101 101 6 5 3 4 3 3 3 4 4 3 1 2 3 3 3 3 3 3 3 3 3 3 4 3 3 2 2 0 18 1 2 2 3 2 2 3 3 1 2 1)

  ,(Ans 6 5 4 2 3 4 4 4 2 3 2 4 4 3 3 2 4 1 2 3 3 1 1 3 3 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Did not answer" 13 2 2 1 0 1 1 0 0 0 1 2 1 2 2 1 3 1 1 1 0 0 0 0 0 4 4 3 3 3 4 2 4 1 1 3 4 101 90 5 3 3 3 3 3 2 2 3 4 5 3 3 5 2 5 5 4 2 4 5 4 4 5 5 4 5 1 18 2 3 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 2 4 4 4 4 4 4 3 3 3 3 3 4 3 3 3 1 3 3 3 1 1 3 3 1 0 0 0 0 4 1 0 7 0 0 1 0 0 0 "Health Studies" 15 2 101 1 0 0 0 0 0 0 1 2 2 1 1 1 2 3 1 1 0 1 0 0 0 4 4 3 2 3 2 3 3 3 3 3 3 101 101 6 5 5 5 5 5 5 5 4 5 3 5 4 4 4 3 3 3 3 3 3 3 3 4 4 4 4 1 18 2 2 1 3 4 4 3 3 3 101 3)

  ,(Ans 6 4 4 1 4 4 4 4 2 4 1 3 4 4 3 1 3 1 4 2 2 2 1 3 4 1 0 0 0 0 3 1 0 0 0 0 1 0 0 1 "English" 12 2 99 1 0 1 0 1 0 0 1 6 3 101 101 101 101 3 0 1 0 0 0 0 0 4 2 3 3 4 4 3 2 2 2 4 4 101 101 6 5 5 5 5 5 4 5 5 5 3 4 5 5 5 4 3 3 3 4 5 3 5 3 4 99 99 0 18 2 3 2 3 3 4 3 3 3 2 3)

  ,(Ans 6 2 4 3 3 4 3 4 4 4 2 3 3 3 3 4 3 3 3 3 3 3 3 3 3 1 0 0 0 0 3 1 1 7 1 1 1 0 0 0 "Undecided" 12 2 101 1 1 1 0 1 0 0 1 6 1 2 2 1 2 1 1 1 0 0 0 0 0 3 3 101 4 101 4 101 101 3 101 3 101 10 30 6 5 5 5 5 5 5 5 5 5 4 5 3 4 3 5 5 3 2 4 3 3 3 5 4 5 5 1 18 2 2 2 3 3 4 3 2 0 0 3)

  ,(Ans 3 5 3 1 4 4 4 2 4 4 4 4 4 4 4 4 4 1 3 1 2 1 2 2 2 1 0 0 0 0 3 1 0 99 1 0 0 0 0 0 "Undecided" 13 2 2 1 0 1 1 0 0 0 1 2 1 3 1 1 1 1 0 0 0 0 1 0 0 3 4 101 3 101 101 101 101 3 3 3 3 15 30 6 1 4 1 2 5 1 2 2 1 2 5 1 3 1 3 3 3 1 2 2 2 2 4 4 3 3 0 18 2 2 2 3 4 4 3 3 0 2 0)

  ,(Ans 6 3 3 1 4 4 4 4 1 1 3 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 4 1 0 7 0 0 1 1 0 0 "Social Work" 13 2 2 1 0 1 0 0 0 0 1 2 3 1 1 1 2 3 0 1 0 0 0 0 1 3 3 3 2 3 3 2 1 1 1 2 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 3 4 4 3 2 2 5 5 5 5 5 5 5 1 18 2 3 3 3 4 3 3 4 3 2 3)

  ,(Ans 6 6 4 1 3 3 3 1 4 4 4 4 2 3 3 4 1 1 1 1 2 2 1 1 3 1 0 0 0 0 4 1 0 2 1 1 1 1 0 0 "Theater Arts" 12 2 2 1 1 0 0 1 0 0 1 1 3 2 2 1 3 1 1 0 0 0 0 0 0 4 3 1 1 3 1 4 1 1 1 2 1 60 90 6 4 4 4 5 5 2 5 4 4 4 2 3 3 2 5 5 4 4 5 5 4 3 5 4 5 5 1 18 1 2 3 3 3 3 3 2 3 0 3)

  ,(Ans 6 6 4 2 4 4 4 4 3 3 2 4 4 3 1 4 3 1 3 3 3 1 1 1 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Communication Studies" 13 2 1 1 0 0 0 0 0 0 1 6 1 1 1 1 3 1 0 1 0 0 0 0 0 4 3 3 1 4 4 4 3 4 1 3 4 101 60 6 5 5 5 5 3 5 5 5 5 5 5 3 4 2 2 4 5 5 5 2 4 3 5 5 5 5 1 18 2 2 2 3 4 3 3 3 3 3 3)

  ,(Ans 6 2 4 1 4 4 4 4 4 4 4 4 4 4 3 4 4 3 4 3 4 4 4 4 4 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Biology" 15 2 3 1 0 1 1 0 0 0 1 6 1 1 2 1 3 1 1 1 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 10 60 6 5 5 4 4 4 2 4 3 3 3 2 3 3 2 3 3 4 2 3 3 3 3 4 5 5 5 1 18 2 2 1 3 4 3 3 3 0 0 3)

  ,(Ans 3 1 2 1 2 4 2 2 3 3 3 2 1 4 2 1 1 1 2 2 1 3 2 2 3 1 0 0 0 0 5 1 1 7 0 0 0 0 0 1 "Philosophy" 15 2 1 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 0 4 3 3 2 3 4 3 1 1 1 2 4 101 101 6 5 5 5 5 5 3 5 5 5 5 5 4 5 4 4 5 2 3 4 5 4 5 3 4 2 2 1 18 2 3 2 3 4 3 3 3 3 2 3)

  ,(Ans 5 2 2 3 4 4 4 4 2 4 4 4 3 2 1 1 1 1 1 1 4 1 3 4 4 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "English" 14 2 1 1 1 1 1 0 0 0 1 6 3 1 1 1 1 3 1 0 1 0 0 0 0 101 101 101 101 101 101 4 3 3 2 4 2 101 101 6 5 5 5 5 5 3 5 5 5 5 5 3 5 2 5 5 2 4 4 5 5 5 4 5 5 5 1 18 2 4 4 3 3 4 3 4 3 2 3)

  ,(Ans 6 3 4 3 4 4 4 3 2 3 4 3 4 4 3 1 3 1 3 4 4 2 1 4 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "International Studies" 15 2 99 1 0 1 1 1 0 0 1 6 3 1 2 1 3 2 1 1 0 0 0 0 0 3 4 4 4 4 4 3 4 4 4 4 4 1 10 6 2 4 5 5 4 4 5 3 3 5 5 3 4 5 5 4 4 5 4 5 5 5 4 5 3 4 1 18 2 5 5 3 3 4 3 3 3 0 3)

  ,(Ans 6 5 3 1 3 3 3 2 2 4 3 1 2 3 2 3 2 1 3 1 2 1 1 1 1 1 0 0 0 0 2 0 1 4 0 1 0 0 0 0 "International Studies" 101 2 101 1 0 0 0 0 0 0 1 1 3 2 2 1 2 1 0 0 0 0 0 0 0 3 3 3 2 3 3 3 2 2 2 3 4 101 45 6 101 5 5 3 5 5 5 3 5 3 5 3 3 3 5 5 3 2 2 5 4 3 3 3 2 2 1 18 1 5 5 3 4 4 2 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 3 3 1 3 1 4 3 1 3 2 2 1 1 2 3 1 0 0 0 0 4 0 0 0 1 0 0 0 0 0 "Business Administration" 12 2 2 1 0 0 0 0 0 0 1 6 3 1 1 1 3 1 0 1 0 0 0 0 0 3 3 3 3 3 4 4 3 4 2 1 3 101 45 6 5 5 5 5 5 4 4 5 4 4 5 2 3 3 3 3 2 2 2 2 3 2 4 4 3 4 1 18 2 2 3 3 4 2 3 3 1 2 1)

  ,(Ans 6 3 3 4 3 4 3 3 2 4 4 4 4 3 2 2 3 2 2 2 2 3 1 3 4 1 0 0 0 0 4 1 0 7 0 0 0 0 0 0 "Environmental Studies" 14 2 101 1 0 0 0 0 0 0 1 6 3 101 101 101 101 3 0 0 0 0 0 1 1 4 3 3 2 3 3 101 101 101 101 101 101 101 101 6 2 2 2 0 3 4 4 3 3 3 3 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 0 18 2 3 2 3 101 101 3 3 1 0 101)

  ,(Ans 5 3 4 4 4 4 4 3 4 4 4 1 1 3 3 101 3 3 3 3 3 1 1 1 1 1 0 0 0 0 3 1 1 1 0 0 0 0 0 0 "Business Administration" 14 2 101 1 0 0 0 0 0 0 1 1 3 3 101 101 101 3 1 0 0 0 1 0 0 4 2 2 2 4 4 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 5 5 3 4 4 4 4 4 4 4 4 1 18 1 2 4 3 3 2 3 3 0 0 1)

  ,(Ans 5 6 2 1 4 4 4 3 1 3 4 4 4 4 2 2 3 1 3 1 2 3 3 3 1 1 1 0 0 0 4 0 0 0 0 0 0 0 0 1 "Foreign Languages" 14 2 2 1 0 1 1 0 0 0 1 6 3 1 2 2 2 3 1 1 0 0 0 0 0 4 2 4 3 4 4 3 1 3 3 4 3 101 101 6 5 5 3 2 0 3 1 0 1 0 0 2 2 4 2 4 4 2 4 4 2 4 5 4 3 2 0 18 1 4 3 3 4 3 3 3 1 1 1)

  ,(Ans 6 3 2 1 2 4 4 4 2 2 3 4 4 3 3 4 2 1 2 3 1 1 1 2 1 1 1 1 1 0 3 1 1 0 0 0 0 0 0 0 "Environmental Studies" 12 2 2 1 0 0 0 0 0 0 1 1 1 3 2 1 2 1 1 1 1 1 1 0 0 4 4 4 2 2 2 4 3 3 3 2 2 10 8 6 3 5 4 3 3 2 5 2 4 5 2 2 5 2 5 5 2 5 2 4 3 3 3 4 3 4 1 18 2 101 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 2 1 3 3 3 3 3 3 3 3 2 4 2 2 1 1 1 3 4 3 1 1 1 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Political Science" 13 2 2 1 0 1 1 0 0 0 1 1 3 1 1 1 1 3 1 1 1 0 0 1 1 3 4 4 4 3 4 3 4 4 3 3 4 101 101 6 5 5 5 5 5 2 5 4 5 5 2 2 3 1 4 5 2 4 2 3 4 4 5 5 4 5 1 18 2 2 2 3 4 2 3 2 1 2 1)

  ,(Ans 3 1 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 4 1 1 7 0 1 0 0 0 0 "Undecided" 12 2 1 0 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 1 0 0 0 0 0 4 4 4 4 4 4 3 3 3 3 3 3 101 101 6 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 1 2 4 2 2 2 2 2 1 0 0)

  ,(Ans 6 5 4 4 4 4 4 4 1 3 3 1 4 2 101 2 3 1 3 3 4 3 4 4 2 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "History" 12 2 1 1 0 1 1 0 0 0 1 1 3 101 101 101 3 1 1 0 0 0 0 0 0 4 3 3 3 4 3 4 3 2 1 4 1 40 60 6 5 5 5 5 5 5 5 5 5 5 5 5 101 3 5 4 5 5 5 5 5 4 4 4 5 5 1 18 1 2 5 3 4 4 3 3 1 1 1)

  ,(Ans 3 6 4 4 1 4 4 4 4 4 4 4 4 3 3 1 1 3 3 3 4 3 2 4 2 1 0 0 0 0 2 1 1 7 0 0 0 0 0 1 "Business Administration" 13 2 2 1 1 1 1 1 0 0 1 1 101 2 101 101 3 1 1 1 1 1 1 1 1 101 101 101 101 101 101 4 4 4 4 4 4 15 20 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 0 18 2 2 4 3 3 3 3 3 1 1 1)

  ,(Ans 6 4 4 1 4 4 3 4 2 2 3 4 4 4 2 1 2 1 3 1 1 1 2 2 3 1 0 0 0 0 5 1 0 0 0 0 0 0 0 0 "International Studies" 17 2 2 1 0 0 0 0 0 0 1 6 3 1 1 2 3 3 0 0 0 0 0 1 0 4 4 3 2 3 4 3 2 2 1 3 4 101 101 6 5 5 5 5 5 5 5 5 5 4 5 5 3 3 3 3 3 5 4 4 2 3 5 4 99 99 99 18 2 4 2 3 4 4 3 3 1 1 1)

  ,(Ans 6 6 4 4 4 4 4 4 3 4 4 3 4 4 3 4 4 4 4 4 4 3 3 4 4 1 0 0 0 0 2 0 1 0 1 0 0 0 0 0 "Business Administration" 12 2 2 1 0 1 1 1 0 0 1 2 1 1 2 1 3 1 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 30 35 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 18 2 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 4 1 3 3 3 2 1 3 2 4 4 2 1 1 1 1 1 1 2 1 1 4 4 0 0 0 1 0 4 1 1 7 0 0 1 0 0 0 "History" 13 2 1 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 1 0 4 3 2 1 3 3 3 2 1 1 2 2 101 101 6 5 5 5 0 3 4 5 5 5 0 5 4 4 4 4 4 3 2 5 4 3 4 4 4 2 3 1 18 1 2 4 3 4 4 3 3 3 2 3)

  ,(Ans 5 6 2 2 4 4 4 3 4 3 2 2 1 2 3 1 3 1 3 2 2 1 3 3 2 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Undecided" 13 2 2 1 1 1 1 1 0 0 1 2 3 1 1 3 3 3 1 0 0 0 0 0 0 3 3 2 3 2 3 3 2 4 3 2 4 101 101 6 3 3 3 2 2 1 3 1 3 4 4 1 3 2 2 3 3 3 2 4 4 3 4 3 2 99 0 18 1 4 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 4 4 1 2 4 4 2 4 3 3 1 4 1 4 2 1 1 1 3 2 1 0 0 0 0 3 1 1 7 0 0 0 0 0 1 "Business Administration" 14 2 2 1 0 1 0 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 1 0 1 3 3 4 2 3 3 3 3 2 2 2 1 101 101 6 5 5 5 3 5 5 5 5 5 5 5 3 4 4 5 5 5 3 3 4 2 4 3 4 3 4 1 18 2 3 1 3 4 4 3 3 1 2 1)

  ,(Ans 6 3 3 4 4 4 4 4 2 3 3 3 4 4 3 1 2 2 2 1 3 1 1 4 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Foreign Languages" 14 2 1 0 0 1 0 1 0 0 1 6 3 1 1 2 2 3 1 0 0 0 1 0 1 3 4 3 3 3 3 3 3 4 4 2 3 101 101 6 2 4 3 5 4 3 5 4 5 3 5 3 3 3 5 5 3 4 3 4 4 3 4 4 3 4 1 18 2 2 2 3 2 3 3 3 1 2 1)

  ,(Ans 6 5 4 2 2 4 4 3 3 4 4 3 4 4 2 3 4 2 4 2 4 4 2 4 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 14 2 2 1 0 1 1 0 0 1 1 2 1 2 2 1 3 1 0 0 0 0 1 0 0 4 4 3 4 4 4 4 4 3 3 4 3 10 15 6 5 5 5 3 5 5 5 0 4 5 5 3 4 2 4 5 4 4 2 3 3 4 4 5 3 4 1 18 1 2 3 3 4 3 3 4 1 0 1)

  ,(Ans 6 6 4 3 4 4 4 2 4 4 4 2 3 4 2 4 4 3 4 2 3 3 3 4 1 1 0 0 0 0 4 0 0 0 0 0 1 0 0 0 "Business Administration" 14 2 2 1 0 1 1 1 0 0 1 1 1 3 2 1 1 1 1 0 0 0 0 0 0 3 3 101 3 101 101 3 101 101 101 2 2 18 35 6 1 3 4 5 3 2 2 2 3 3 2 1 2 2 3 4 2 3 2 2 2 3 4 3 3 3 1 18 2 2 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 4 4 1 3 4 4 1 4 3 3 4 3 3 1 3 3 1 2 1 1 1 2 1 4 1 0 0 0 0 4 1 1 2 0 0 0 0 0 1 "Music" 13 2 2 1 0 1 1 0 0 0 1 3 1 1 1 1 3 1 1 0 0 0 0 0 0 4 4 4 4 4 4 3 2 2 3 3 3 35 55 6 5 5 4 5 5 5 5 5 5 3 3 4 3 4 3 3 4 2 4 3 3 3 4 4 4 4 1 18 1 99 4 3 4 4 2 3 1 1 1)

  ,(Ans 5 3 4 4 4 4 4 4 101 2 4 3 3 1 2 2 2 2 3 4 4 3 1 4 3 0 1 0 0 0 4 1 1 101 0 0 0 0 0 0 "Undecided" 13 2 101 1 0 0 0 0 0 0 1 3 3 101 101 101 2 3 0 0 0 0 0 0 0 4 3 3 3 3 3 4 3 3 3 3 3 101 101 6 2 5 5 5 3 2 5 3 5 5 5 3 3 5 4 3 3 3 2 3 3 4 3 3 4 4 1 18 1 5 5 3 101 101 3 3 1 0 1)

  ,(Ans 5 5 3 3 4 4 4 3 3 3 4 3 3 3 4 3 3 2 3 3 3 2 2 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Environmental Sciences and Resources" 13 2 2 1 0 1 1 1 0 0 1 1 101 101 101 101 3 1 1 0 0 0 1 0 0 3 3 4 3 3 4 2 2 3 2 3 3 7 25 6 5 5 3 5 5 3 3 4 5 3 3 3 3 2 3 4 3 2 3 3 2 3 4 3 4 4 0 18 1 2 2 3 3 4 3 3 0 0 1)

  ,(Ans 6 5 4 1 4 4 4 4 4 3 3 1 2 2 1 3 3 1 1 1 3 3 2 3 1 1 0 0 1 0 3 1 1 0 1 0 1 0 0 0 "Mathematics" 13 2 2 1 0 1 1 1 0 0 1 6 101 101 101 101 3 1 1 1 0 0 0 0 0 3 2 2 1 3 4 4 2 2 1 4 4 101 45 6 2 5 5 0 5 2 1 1 4 101 5 1 2 3 3 3 4 2 4 4 2 4 3 5 4 4 1 18 1 2 2 3 4 3 3 2 0 101 1)

  ,(Ans 6 5 4 1 3 2 3 2 1 3 4 2 1 4 1 1 3 2 4 1 3 1 1 2 1 1 0 0 0 1 4 1 1 99 0 1 1 0 0 0 "Business Administration" 15 2 2 0 0 1 0 1 0 0 1 6 3 101 2 2 3 3 0 0 1 0 0 0 0 2 2 3 3 3 3 4 2 2 1 1 2 101 101 6 4 5 4 3 5 5 5 2 5 4 5 2 3 3 5 5 3 3 3 3 3 4 4 3 5 5 0 18 2 2 4 3 4 3 3 3 1 1 1)

  ,(Ans 6 6 4 2 4 3 4 2 3 4 3 3 4 4 3 1 4 1 4 3 2 1 2 3 2 1 0 0 0 0 2 1 1 0 0 1 0 0 0 0 "Business Administration" 14 2 2 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 1 1 0 0 0 0 0 3 4 4 3 3 4 3 4 4 3 3 4 101 101 6 3 5 2 3 3 3 3 4 4 2 3 2 3 2 4 4 3 3 1 2 3 3 5 4 3 3 1 18 2 3 4 3 4 3 3 3 1 2 1)

  ,(Ans 6 4 4 2 4 3 4 4 3 4 3 3 4 4 4 1 4 1 3 3 3 1 2 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Health Studies" 12 2 1 1 0 0 0 0 0 0 1 6 3 101 101 101 101 2 0 0 0 0 0 0 0 3 2 3 2 3 3 3 2 2 2 3 3 1 5 6 3 5 5 2 5 3 5 5 5 2 5 2 4 4 4 4 4 2 3 4 3 4 3 4 3 3 1 18 2 2 4 3 3 4 4 2 1 0 1)

  ,(Ans 6 6 4 1 4 4 4 4 3 4 4 1 4 4 4 1 2 1 4 2 4 1 1 4 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Business Administration" 13 2 2 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 1 0 0 1 1 0 3 3 3 2 2 2 3 3 2 1 1 1 101 101 6 5 5 5 2 2 5 5 5 5 0 5 3 4 2 4 4 4 2 3 4 4 4 4 4 99 99 1 18 2 2 4 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 4 1 3 4 1 2 2 3 3 2 4 1 1 3 3 1 2 1 2 1 3 1 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "English" 16 2 2 1 0 1 0 0 0 1 1 3 101 101 2 3 3 1 0 1 0 0 0 0 0 4 3 3 2 4 3 4 2 2 3 2 2 10 60 6 2 5 5 4 5 3 2 4 4 5 2 3 5 5 5 4 3 3 3 4 4 5 4 4 3 3 1 18 1 2 4 3 3 3 3 2 1 0 1)

  ,(Ans 6 3 3 1 4 4 4 3 2 3 4 4 3 4 2 1 3 1 2 1 4 1 1 3 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Environmental Studies" 12 2 3 1 0 1 1 0 0 0 1 6 2 101 101 2 3 2 1 1 0 0 0 0 0 4 4 3 3 3 4 4 2 2 2 2 3 7 50 6 5 5 4 5 5 4 5 4 5 5 4 5 5 3 4 5 3 3 4 4 5 5 5 5 5 5 1 18 2 5 5 3 4 3 3 3 1 2 1)

  ,(Ans 99 1 2 3 3 4 4 4 3 4 4 4 4 4 4 4 4 1 4 4 4 4 4 4 2 1 0 0 0 1 5 0 1 7 0 0 0 0 0 1 "Did not answer" 14 2 2 1 1 1 0 0 0 0 2 2 1 1 1 1 1 1 1 1 0 0 0 1 0 4 4 4 4 4 4 101 101 101 101 101 101 7 30 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 18 2 2 3 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 4 4 3 2 3 4 3 3 4 4 1 4 2 4 4 3 2 1 3 4 1 0 0 0 0 1 1 0 3 0 0 1 0 0 0 "Environmental Studies" 12 2 3 1 0 1 0 1 0 0 1 6 1 1 2 1 3 1 1 1 0 1 0 1 0 101 101 101 101 101 101 4 4 3 3 4 4 18 50 6 5 5 5 5 5 5 5 3 5 4 5 3 4 3 5 5 3 2 2 4 3 3 4 5 4 4 1 18 2 3 2 3 4 4 3 3 3 0 3)

  ,(Ans 3 3 3 4 4 4 4 4 2 4 3 3 4 4 4 1 3 3 3 3 2 2 2 3 3 1 0 0 0 0 4 1 0 0 0 0 0 0 0 0 "Business Administration" 15 2 1 1 0 1 1 1 0 0 1 1 3 1 1 1 2 3 1 0 0 1 1 0 0 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 5 5 5 5 5 5 5 4 5 5 5 4 4 2 5 5 3 4 4 4 4 3 4 3 1 1 1 18 1 3 2 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 2 3 4 4 2 4 4 4 1 3 3 3 2 2 2 4 4 2 1 1 0 0 0 5 1 1 0 0 0 0 0 0 1 "Did not answer" 15 2 2 1 0 1 1 1 0 1 1 1 3 1 2 1 2 3 1 0 0 1 0 0 0 4 3 4 2 4 4 3 3 3 1 3 3 101 101 6 5 5 5 5 5 2 1 5 5 2 2 3 3 3 3 3 3 3 3 3 3 3 4 4 3 3 1 18 1 3 3 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 4 2 4 4 4 4 4 4 2 3 4 4 4 3 4 1 4 3 3 2 1 1 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 12 2 2 1 1 1 1 1 0 0 1 1 2 2 101 101 3 1 1 1 1 1 0 1 0 4 4 4 1 3 4 3 3 2 1 2 4 101 20 6 1 3 2 5 5 3 3 1 1 0 1 1 1 1 4 4 4 5 2 1 1 1 3 4 4 4 1 18 2 2 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 3 3 4 4 3 4 2 4 3 4 3 2 1 1 3 1 4 2 2 2 2 2 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Psychology" 16 2 101 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 0 1 4 4 4 4 3 3 3 3 3 3 3 3 101 101 6 3 5 3 3 5 2 3 2 4 3 2 2 3 2 4 4 3 5 3 2 3 2 4 4 4 4 1 18 2 4 4 3 4 3 3 3 3 2 3)

  ,(Ans 3 5 3 1 3 4 4 4 4 3 3 4 2 3 3 1 3 1 3 3 4 4 4 4 4 1 1 0 0 0 2 1 1 0 0 0 1 0 0 0 "Undecided" 14 2 2 1 0 1 1 1 0 0 1 6 3 101 101 3 101 3 0 1 0 0 0 1 0 4 4 4 4 3 4 4 3 4 4 3 4 101 101 6 0 5 5 5 5 5 5 4 5 5 5 1 3 3 4 4 3 1 2 4 4 3 4 5 3 3 1 18 2 4 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 4 3 3 3 4 4 3 1 4 4 3 3 3 4 1 3 3 3 3 3 3 101 4 1 0 1 1 0 0 3 1 1 4 0 0 1 0 0 0 "Undecided" 13 2 2 1 0 0 0 0 0 0 1 3 3 1 2 2 3 3 1 1 0 0 0 0 1 4 3 3 3 3 3 101 101 101 101 101 101 101 101 6 0 4 4 2 2 3 5 3 3 2 5 1 1 2 2 2 1 1 1 1 1 1 5 5 5 101 1 18 1 5 3 3 3 2 2 3 3 3 3)

  ,(Ans 3 5 3 1 4 4 4 2 3 2 4 3 3 3 3 1 2 1 3 2 3 2 2 2 1 0 0 0 1 0 3 0 0 0 0 0 1 1 0 0 "" 12 2 99 1 0 1 1 1 0 0 1 3 3 1 1 3 3 3 1 1 0 0 0 1 1 101 101 101 101 101 101 4 4 2 2 1 2 101 101 6 0 4 4 2 2 5 5 1 1 1 5 1 2 3 5 5 4 1 101 2 3 2 4 3 4 4 101 18 2 5 3 3 4 4 2 2 3 3 3)

  ,(Ans 6 3 4 4 2 3 3 3 101 3 4 3 3 4 4 4 4 1 4 1 1 1 1 1 1 0 1 0 0 0 2 1 0 0 1 1 1 0 0 0 "Music" 14 2 2 1 0 1 0 0 0 0 1 1 1 3 2 1 1 1 0 0 0 0 0 0 0 4 4 4 101 4 101 101 101 101 101 101 101 25 40 6 3 3 2 5 5 2 3 1 3 0 2 2 3 2 4 4 4 1 1 2 1 2 4 4 3 2 0 18 2 2 2 3 4 4 101 3 3 0 3)

  ,(Ans 6 2 3 3 4 4 3 4 1 3 4 3 3 3 2 1 3 1 3 3 3 2 1 2 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 16 2 99 1 1 1 0 1 0 0 1 3 3 1 1 3 2 3 1 0 0 0 1 1 0 4 3 4 1 3 4 4 2 3 1 3 4 101 101 6 5 5 2 5 5 3 5 3 3 3 5 5 5 2 4 4 2 3 2 3 3 3 4 4 3 3 0 18 1 2 5 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 4 4 3 2 4 2 4 4 2 3 3 2 4 2 1 2 1 2 1 1 3 1 1 1 0 0 0 3 1 1 0 0 0 0 0 0 1 "Music" 14 2 3 1 0 1 1 0 0 0 1 2 1 1 1 1 3 1 0 1 0 0 0 0 0 3 2 2 2 2 2 3 2 1 2 2 1 50 60 6 5 5 5 5 2 3 4 5 5 4 5 5 5 5 4 3 4 3 5 5 4 5 4 4 4 4 0 18 2 3 4 3 4 4 3 3 3 3 3)

  ,(Ans 5 6 2 1 4 3 4 2 3 4 4 2 4 3 4 1 3 1 4 1 1 1 1 1 3 1 0 0 0 0 2 1 0 7 0 0 1 0 0 0 "Psychology" 16 2 99 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 3 4 4 4 3 3 2 3 2 101 101 6 5 5 5 3 5 5 5 0 4 5 101 5 5 4 4 4 4 3 1 3 5 4 4 4 4 5 1 18 2 4 4 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 2 2 1 1 4 1 4 1 4 2 1 2 1 3 1 3 1 3 1 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Business Administration" 14 2 1 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 3 2 3 4 3 3 1 1 2 2 3 101 101 6 5 5 4 5 4 5 5 3 4 5 5 4 3 3 3 4 5 4 4 4 3 3 1 1 1 1 1 18 1 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 3 3 4 4 4 1 4 4 4 3 3 3 2 1 3 2 3 3 3 1 2 2 4 1 0 0 0 0 3 1 1 7 1 0 1 0 0 0 "Business Administration" 14 2 101 1 0 0 0 0 0 0 1 6 3 1 2 2 2 3 0 0 0 0 1 1 0 3 2 3 3 3 3 2 3 3 3 3 3 101 101 6 3 3 4 5 3 0 4 0 2 3 3 3 2 2 4 4 1 1 1 1 2 2 3 3 2 3 1 18 2 2 1 3 4 4 3 3 3 0 101)

  ,(Ans 6 6 3 3 3 4 4 3 3 3 3 4 2 3 3 1 3 3 3 3 3 2 2 2 3 0 0 0 1 0 3 1 1 7 0 0 0 0 0 0 "Business Administration" 13 2 1 1 0 1 0 0 0 0 1 3 3 1 2 1 1 3 1 0 0 0 1 0 0 3 3 3 3 3 3 3 3 3 3 2 2 101 101 6 2 4 5 5 3 2 2 2 3 3 2 1 2 2 3 3 2 2 2 2 1 2 3 3 3 3 1 18 1 2 3 3 3 2 3 3 3 2 3)

  ,(Ans 6 5 4 4 4 3 3 2 4 4 4 3 3 3 3 4 2 2 2 2 2 4 3 3 4 1 0 0 0 0 4 1 0 2 0 0 0 0 0 1 "Business Administration" 12 2 2 1 0 1 1 1 0 0 1 6 2 2 2 1 3 1 1 0 0 0 1 0 0 3 3 3 2 2 2 3 3 3 2 2 2 10 45 6 5 5 5 4 5 5 5 5 5 5 5 4 4 4 4 5 5 5 3 4 3 4 3 3 3 3 1 18 1 2 3 3 4 4 3 3 3 2 3)

  ,(Ans 6 4 4 2 4 4 3 4 2 4 1 2 3 3 2 4 2 1 3 1 1 1 1 1 1 1 1 0 0 0 4 0 1 0 0 0 0 0 0 1 "Biology" 14 2 2 1 0 1 1 1 0 0 1 2 3 2 1 1 3 1 0 0 0 0 0 0 0 4 2 3 2 4 4 2 1 2 2 3 4 18 40 6 4 4 4 5 5 4 5 5 4 5 5 3 3 3 5 5 4 4 4 4 4 4 5 5 5 5 1 18 1 4 4 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 3 1 3 3 4 3 3 4 4 4 4 4 1 4 1 1 1 1 1 1 2 1 4 1 0 0 0 0 3 0 0 7 0 0 1 0 1 0 "Architecture" 16 2 1 1 1 1 1 1 0 0 1 6 3 1 1 2 3 1 0 0 0 0 1 0 0 4 2 2 2 2 4 4 1 101 1 2 1 30 75 6 5 5 3 5 5 1 1 2 5 5 1 3 3 3 5 4 2 1 2 2 101 101 5 4 101 3 1 18 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 3 6 3 1 4 4 4 3 2 4 3 3 4 4 3 1 3 1 4 2 2 1 1 4 3 1 0 0 0 0 5 0 1 0 0 0 0 0 0 1 "Communication Studies" 13 2 2 1 0 1 0 1 0 0 1 6 3 1 1 1 2 3 1 0 0 0 0 0 1 4 2 3 3 3 4 2 1 1 2 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 4 3 4 4 4 4 5 5 4 5 4 3 4 5 1 18 2 3 2 3 3 3 3 2 1 1 1)

  ,(Ans 6 3 3 1 4 4 4 4 2 4 3 4 3 4 4 1 3 1 4 3 3 1 1 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "International Studies" 13 2 101 1 0 0 0 0 0 0 1 4 3 1 1 2 1 3 1 0 0 0 0 0 1 4 3 2 3 3 1 4 3 1 2 4 1 101 101 6 2 2 5 1 5 5 5 5 5 5 5 3 1 3 4 4 4 3 3 4 3 4 3 4 3 4 1 18 2 3 5 3 4 2 3 2 1 0 1)

  ,(Ans 3 5 4 1 4 4 3 2 1 4 4 3 4 3 1 2 1 101 2 1 1 1 2 1 1 1 0 0 0 0 4 1 0 0 0 0 0 0 0 1 "Foreign Languages" 12 2 99 1 0 1 1 1 0 0 1 2 3 101 101 101 2 3 1 0 0 0 0 0 1 4 4 3 3 4 4 4 3 2 2 4 4 101 101 6 5 5 3 5 3 4 5 0 3 0 3 5 5 3 4 5 3 5 1 4 3 3 4 4 99 99 1 18 2 4 4 3 3 4 2 3 1 2 1)

  ,(Ans 6 5 4 2 3 4 4 2 1 1 3 3 3 1 2 1 1 1 1 1 1 1 1 2 3 1 0 0 0 0 4 0 1 99 0 0 0 0 0 0 "Undecided" 13 2 99 1 0 1 1 1 0 0 1 6 3 101 101 101 101 3 1 0 0 0 0 0 0 4 4 3 3 4 4 3 2 2 1 3 4 101 101 6 5 5 5 4 4 4 5 4 5 3 5 4 4 3 5 5 4 4 3 4 3 5 4 5 3 3 1 18 1 5 5 3 3 2 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 3 4 4 3 4 3 3 1 3 3 4 4 2 3 1 2 1 1 1 1 1 0 0 0 0 4 0 1 0 0 1 1 1 0 0 "Elementary Education" 12 2 2 1 0 0 0 0 0 0 1 2 1 3 1 1 2 1 0 0 0 0 0 0 0 3 1 3 4 3 4 2 1 2 3 3 3 15 35 6 5 5 5 0 5 3 4 3 5 5 2 3 5 2 2 2 3 1 3 5 5 5 4 4 2 4 1 18 2 2 2 3 3 3 3 3 3 3 3)

  ,(Ans 6 5 4 3 4 4 4 3 4 4 4 4 3 4 4 3 4 3 4 3 3 3 3 3 4 1 0 0 0 0 4 0 0 4 0 1 0 0 0 0 "Business Administration" 13 2 101 1 0 1 1 0 0 0 1 3 3 101 101 101 3 2 1 0 0 0 1 0 0 3 4 2 1 4 3 3 4 3 2 4 3 5 10 6 2 5 4 5 5 4 5 5 4 1 4 2 3 3 2 2 3 2 3 3 3 3 4 4 4 4 1 18 1 3 2 3 2 4 2 3 3 0 3)

  ,(Ans 6 3 3 1 4 4 3 2 2 4 4 2 4 1 2 1 1 3 2 1 2 1 4 3 1 1 0 0 0 0 4 0 0 7 0 0 1 0 0 0 "Political Science" 13 2 99 1 1 1 1 1 0 0 1 6 2 1 1 3 1 3 1 0 0 0 1 0 1 4 2 1 4 1 4 4 3 1 3 1 4 101 101 6 5 5 5 3 4 5 5 5 5 2 5 3 3 2 5 5 5 4 4 3 4 3 5 5 4 5 1 18 1 3 4 3 3 2 3 3 3 0 3)

  ,(Ans 5 2 3 1 4 4 4 4 4 4 4 2 4 2 3 1 2 2 2 2 2 2 2 2 1 1 0 0 0 0 4 0 0 99 0 0 1 0 0 0 "Health Studies" 17 2 2 1 0 0 0 0 0 0 1 1 1 3 2 1 1 2 0 0 0 0 1 0 0 3 3 3 3 3 3 101 101 101 101 101 101 2 10 6 5 5 5 5 5 5 5 5 5 5 5 3 2 3 3 3 2 3 3 2 3 3 4 4 3 4 1 18 2 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 1 3 3 3 3 3 3 4 3 4 4 4 2 2 4 3 3 3 3 3 3 1 3 3 0 0 0 1 0 2 1 1 7 0 0 1 0 0 0 "Undecided" 13 2 1 1 0 0 0 0 0 0 1 6 1 1 3 1 1 1 1 1 0 0 0 0 0 3 3 2 3 2 2 101 101 101 101 101 101 25 30 6 2 4 2 5 4 4 4 2 2 3 3 3 3 3 4 3 3 3 3 3 3 3 3 4 4 4 1 18 2 2 3 3 4 3 3 3 1 1 1)

  ,(Ans 3 5 3 2 3 4 4 2 3 2 2 2 2 2 3 4 4 1 3 2 1 1 3 2 2 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "English" 12 2 2 1 0 0 1 0 0 0 1 1 1 3 1 1 1 1 0 0 0 0 0 0 0 3 2 3 2 3 3 3 2 3 2 3 3 23 30 6 5 5 5 0 2 2 2 5 5 2 5 3 3 2 3 3 3 1 3 3 2 3 2 3 2 2 0 18 1 2 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 1 3 3 4 4 4 4 2 4 2 4 3 4 4 3 3 3 3 3 4 2 2 2 4 1 0 0 0 0 4 1 1 7 1 0 0 0 0 0 "Business Administration" 13 2 101 1 0 1 1 1 0 1 101 3 3 1 2 1 3 3 0 0 0 0 0 0 0 101 101 101 101 101 101 3 3 3 3 3 3 101 101 6 5 5 5 5 5 4 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 3 5 3 3 0 18 1 2 1 3 3 3 3 3 1 0 1)

  ,(Ans 99 2 4 4 2 4 4 4 4 4 3 2 4 4 4 1 4 1 4 3 3 2 1 3 4 0 0 0 0 1 4 0 1 1 0 0 1 0 0 1 "Business Administration" 13 2 2 1 0 1 0 0 0 1 2 6 2 2 2 1 2 2 1 1 0 0 0 0 0 4 3 3 3 3 2 4 3 2 2 2 2 15 30 6 5 5 4 5 5 5 4 3 5 3 5 3 3 4 3 3 101 4 2 3 2 3 4 4 3 2 0 18 2 2 2 3 3 3 2 2 1 0 1)

  ,(Ans 6 6 4 1 4 4 4 4 1 4 3 2 101 3 3 1 2 1 3 3 4 1 1 3 3 1 0 0 0 0 3 0 1 0 0 0 0 1 0 0 "Undecided" 13 2 101 1 0 1 0 0 0 0 1 6 1 1 1 2 3 2 1 1 0 0 0 1 0 4 2 3 1 1 3 4 2 3 1 1 3 5 19 6 5 4 4 1 2 3 4 4 5 5 5 3 2 2 4 4 3 1 3 3 3 3 5 5 4 4 0 18 1 1 3 3 3 4 3 3 1 1 1)

  ,(Ans 5 6 4 3 4 4 101 4 2 4 4 3 1 3 3 3 3 2 3 3 2 2 2 3 3 1 0 0 0 0 3 1 1 0 0 0 0 0 1 0 "Computer Science" 16 2 2 1 0 1 1 1 0 0 1 2 3 2 2 1 2 1 1 0 0 0 0 0 0 1 1 101 2 101 3 4 4 3 3 4 3 10 15 6 5 5 5 3 5 5 5 5 5 5 5 3 5 4 4 4 5 3 4 4 3 5 4 4 3 4 0 18 1 2 4 3 3 3 3 3 1 0 1)

  ,(Ans 6 4 4 2 3 4 4 4 3 4 4 101 4 4 3 1 4 4 4 3 4 4 1 4 3 1 0 0 0 0 4 0 0 0 0 0 0 0 1 0 "Electrical Engineering" 16 2 1 1 0 0 0 0 0 0 1 3 3 1 2 2 3 3 1 1 0 0 1 1 0 4 4 3 3 3 3 4 3 3 3 3 3 101 101 6 101 0 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 3 4 4 4 4 5 5 4 5 1 18 1 3 5 3 3 4 2 3 0 0 0)

  ,(Ans 99 4 3 2 2 3 2 4 2 3 3 4 2 4 3 4 3 1 3 2 3 1 2 2 2 1 0 0 0 0 3 1 1 0 0 0 0 0 1 0 "Mechanical Engineering" 18 2 2 1 0 0 1 0 0 1 1 3 3 1 1 1 3 1 1 1 0 0 0 0 0 3 3 3 3 3 3 101 101 101 101 101 101 16 70 6 1 1 1 0 4 2 3 4 1 1 1 3 2 3 3 3 3 2 4 3 3 2 4 4 4 4 1 18 1 5 4 3 3 2 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 4 4 4 2 4 4 4 4 4 4 4 4 2 4 4 4 1 2 4 4 1 0 0 0 0 5 0 1 0 0 0 1 0 0 0 "" 101 2 2 1 0 1 1 1 0 0 1 3 1 2 2 1 3 1 1 1 0 0 1 0 0 3 3 2 101 1 1 101 101 101 101 101 101 20 70 6 5 5 5 101 101 5 5 5 5 5 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 1 18 1 4 5 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 2 3 3 4 3 3 4 3 3 4 3 3 1 2 3 3 2 2 2 1 2 2 1 0 0 0 0 4 0 0 0 1 0 0 0 0 0 "Business Administration" 13 2 101 1 0 1 1 0 0 0 1 3 3 101 101 101 101 3 1 1 0 0 1 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 2 5 5 3 5 2 4 0 2 0 5 2 2 3 3 3 2 3 2 2 2 3 4 4 4 3 0 18 2 4 4 3 3 2 3 3 1 0 1)

  ,(Ans 3 4 3 4 4 4 4 4 2 3 4 4 3 4 4 4 3 2 4 4 4 3 3 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 1 0 "Psychology" 12 2 2 1 0 1 1 0 0 0 3 6 1 1 2 2 3 1 0 1 0 0 0 0 0 1 1 1 1 3 3 101 101 101 101 101 101 20 60 6 5 5 5 5 5 5 5 5 5 5 3 1 2 4 5 5 5 2 1 2 2 1 5 5 4 5 1 18 2 1 1 2 4 3 3 2 3 2 3)

  ,(Ans 6 3 3 2 4 4 4 4 3 4 3 3 3 4 3 3 3 2 3 3 4 3 3 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Art" 12 2 2 1 1 1 0 0 0 0 1 2 3 1 2 2 2 3 1 0 0 1 0 0 0 4 4 101 3 4 3 4 4 101 3 3 3 101 101 5 5 5 5 5 5 5 5 4 3 3 5 5 5 3 5 5 5 4 4 4 4 4 4 4 3 3 1 18 1 2 5 3 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 2 4 3 4 4 4 4 4 3 3 4 3 1 3 2 3 2 3 2 3 4 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Undecided" 16 2 3 1 0 1 1 1 0 0 1 1 3 1 1 1 2 3 1 1 0 0 1 0 0 4 4 4 3 4 3 3 3 3 2 3 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 3 5 5 5 4 5 5 5 5 4 5 4 4 1 18 2 5 5 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 3 4 3 3 4 4 4 2 3 3 3 3 1 3 4 1 0 0 0 0 3 1 1 7 1 0 1 0 0 0 "Business Administration" 13 2 2 1 0 1 1 0 0 0 1 6 2 1 2 1 3 1 0 0 0 1 0 0 0 3 3 4 4 3 3 3 2 101 3 101 101 15 35 6 5 5 5 3 3 4 4 3 3 5 3 4 3 3 3 3 3 3 3 3 4 3 4 4 4 3 1 18 1 2 1 3 4 3 3 3 3 0 3)

  ,(Ans 3 6 3 4 4 4 4 3 4 4 3 3 2 4 3 3 3 1 1 1 2 3 3 2 3 1 0 0 0 1 2 1 1 0 0 0 0 0 1 0 "Mechanical Engineering" 18 2 2 1 0 1 1 1 0 0 1 2 1 3 1 1 2 1 0 0 0 0 1 0 0 3 2 1 1 3 3 3 1 1 1 3 3 20 30 6 5 5 2 5 5 5 5 5 5 2 3 3 4 3 4 4 3 4 4 5 4 5 4 4 3 3 0 18 1 1 2 3 4 4 3 3 0 0 0)

  ,(Ans 6 6 4 4 4 4 2 2 1 4 4 4 4 4 1 2 3 2 4 3 2 2 4 4 3 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Mechanical Engineering" 15 2 2 1 1 1 1 1 0 0 1 1 1 1 2 1 3 1 1 0 0 0 1 0 0 4 3 1 1 4 4 4 2 1 3 4 4 15 15 6 5 5 2 5 5 5 5 4 4 3 5 2 3 4 5 5 5 5 4 3 3 5 5 4 4 5 1 18 1 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 3 4 2 4 3 2 3 4 4 4 4 4 4 2 1 3 1 4 3 3 2 1 4 4 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Mechanical Engineering" 14 2 1 0 0 1 1 0 0 1 1 6 3 2 1 2 1 3 0 0 0 0 0 0 1 4 3 3 3 4 4 4 3 2 2 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 5 4 5 4 5 4 4 4 4 3 3 1 18 1 2 1 3 3 3 2 3 3 3 3)

  ,(Ans 6 6 3 4 4 3 3 3 2 3 2 3 3 3 3 3 3 3 3 3 3 4 2 2 4 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Business Administration" 16 2 2 1 0 1 0 0 0 0 1 3 1 2 3 1 2 1 1 0 0 0 0 0 0 3 3 4 3 3 2 3 3 4 3 3 2 20 30 6 5 5 5 5 4 5 5 4 5 5 5 3 3 4 4 4 4 2 3 4 4 4 4 4 3 4 1 18 2 3 3 3 3 2 3 3 0 0 0)

  ,(Ans 6 6 4 1 4 4 4 1 3 3 3 4 4 3 3 1 2 1 3 3 2 1 2 3 2 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Did not answer" 17 2 2 1 0 1 0 1 0 0 1 6 3 1 1 3 1 3 1 0 0 0 1 0 0 3 4 4 2 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 4 5 4 3 2 3 3 3 2 3 4 3 3 4 4 3 3 1 18 2 3 4 3 3 4 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 1 3 4 4 1 2 3 2 4 3 2 3 3 3 2 2 1 3 1 0 0 0 0 3 1 0 0 1 0 0 0 0 0 "Engineering" 13 2 2 1 0 1 1 1 0 0 1 3 2 2 101 101 3 1 1 0 0 0 0 0 0 3 3 3 2 3 2 3 3 3 2 3 2 13 60 6 5 5 5 3 5 4 4 2 2 2 5 3 3 3 3 4 4 2 3 3 3 3 4 3 3 3 0 18 1 2 4 3 4 2 2 3 3 0 3)

  ,(Ans 6 5 3 1 4 4 4 2 4 3 3 4 4 3 3 1 3 1 3 2 4 3 1 4 3 1 0 0 0 0 4 0 1 7 1 0 0 0 0 0 "Environmental Studies" 15 2 99 1 0 0 0 0 0 0 1 6 3 101 101 101 101 3 1 1 0 0 1 1 1 101 101 101 101 101 101 4 3 4 4 4 4 101 101 6 5 5 5 5 5 4 5 5 5 5 5 3 3 2 4 4 4 5 4 4 5 3 5 5 5 5 1 18 2 3 4 3 4 4 3 2 3 2 3)

  ,(Ans 5 3 3 1 4 4 4 3 1 4 4 3 3 2 1 1 1 4 3 2 3 1 1 3 1 1 1 0 0 0 4 0 1 99 0 0 0 0 0 1 "Undecided" 15 2 1 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 1 0 0 1 0 0 4 4 4 3 4 4 4 3 2 1 3 4 101 101 6 5 5 5 3 5 1 5 2 5 4 5 4 4 4 3 3 2 2 3 4 4 4 5 5 3 3 1 18 1 5 5 3 3 4 3 3 3 2 3)

  ,(Ans 6 6 3 1 4 4 4 1 3 4 3 2 2 3 3 4 3 1 3 2 3 3 1 2 1 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Accounting" 15 2 99 1 0 1 0 1 0 0 1 1 1 2 2 2 3 1 1 0 0 0 0 0 0 2 1 1 3 1 2 2 2 2 1 3 3 10 15 6 2 5 5 2 5 3 3 2 2 0 5 2 3 4 4 4 4 3 3 3 3 5 3 3 2 4 1 18 1 4 2 3 3 3 3 3 0 2 0)

  ,(Ans 6 6 4 4 4 4 4 3 1 4 3 4 4 3 2 1 3 2 3 1 3 1 1 4 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Art" 15 2 1 1 0 1 1 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 1 0 0 4 3 3 2 4 4 4 3 3 2 4 4 101 101 6 5 5 5 3 4 1 2 2 4 1 4 3 4 3 5 5 3 1 3 4 3 4 4 4 3 4 1 18 2 4 2 3 4 4 3 4 3 0 3)

  ,(Ans 5 3 2 1 2 2 3 2 3 3 3 2 4 3 2 1 2 1 1 1 1 1 2 2 2 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Physics" 13 2 101 1 0 1 1 1 0 0 1 6 3 1 2 2 1 3 1 0 0 0 0 0 0 4 3 2 1 3 3 4 3 2 1 3 4 101 101 6 2 5 5 3 5 5 4 3 5 4 5 3 4 4 4 4 4 3 4 3 3 3 4 4 99 99 1 18 1 5 2 3 4 3 3 2 3 2 3)

  ,(Ans 6 6 4 2 4 4 3 1 4 4 4 4 2 4 2 3 3 2 3 3 1 1 2 1 4 1 1 0 0 0 3 1 1 0 0 0 0 0 0 1 "Did not answer" 13 2 2 1 0 1 0 1 0 0 1 2 1 2 1 1 3 1 1 0 0 0 0 0 0 4 3 3 3 3 2 4 3 3 3 3 2 15 25 6 5 5 5 5 5 5 5 5 5 2 3 4 4 4 4 5 5 5 4 4 2 4 99 99 99 99 0 18 1 99 99 3 4 3 3 3 1 0 1)

  ,(Ans 6 6 4 2 4 4 4 3 3 3 3 3 4 3 4 1 3 1 3 3 3 3 3 3 2 1 0 0 0 0 4 0 1 7 0 0 1 0 0 0 "Psychology" 14 2 2 1 1 1 1 1 0 0 1 2 3 2 2 1 1 3 0 0 0 0 0 0 1 1 1 1 1 1 1 101 101 101 101 101 101 101 101 6 3 5 5 5 5 5 5 5 5 5 5 3 3 3 4 5 3 4 3 3 3 3 5 5 4 5 1 18 2 2 3 3 3 3 3 3 0 0 0)

  ,(Ans 3 4 4 3 3 4 3 4 2 3 3 4 4 2 2 4 4 1 3 2 3 2 2 2 2 0 0 0 1 0 3 1 1 0 0 1 1 0 0 0 "Psychology" 13 2 2 1 0 1 0 1 0 0 1 3 3 1 1 3 3 1 0 0 0 0 0 0 0 4 3 4 2 3 4 4 2 4 1 3 2 7 45 6 2 5 4 1 2 4 5 4 5 1 3 3 4 2 2 2 4 3 4 4 3 5 4 3 2 3 1 18 1 2 2 3 3 3 3 3 1 1 1)

  ,(Ans 5 4 1 1 1 4 4 4 4 1 2 4 3 2 2 1 2 1 2 1 1 1 1 2 4 0 0 0 0 1 5 0 1 7 0 0 0 0 1 0 "History" 21 2 2 1 0 1 1 1 0 0 1 6 2 1 1 2 3 2 0 0 0 0 0 1 0 4 3 3 1 3 3 3 2 2 1 1 2 2 34 6 5 5 5 5 5 5 5 5 5 5 5 5 3 4 3 3 4 5 3 4 5 3 3 5 5 3 1 18 2 4 4 3 3 4 3 3 1 2 0)

  ,(Ans 3 3 3 4 4 4 4 101 4 4 4 2 4 2 2 1 3 1 3 3 4 2 1 4 2 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 14 2 99 0 0 1 0 0 0 1 1 1 3 1 1 1 1 3 0 1 0 0 0 0 1 4 3 4 2 4 4 4 2 3 2 3 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 2 3 4 4 4 3 3 3 3 4 4 4 4 4 4 1 18 2 4 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 5 1 3 3 4 3 3 3 4 4 4 4 4 3 1 3 1 3 3 3 1 1 4 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 13 2 101 1 0 0 0 0 0 0 1 2 3 101 101 101 101 3 0 0 0 0 0 0 0 4 3 4 3 4 4 3 3 3 2 3 4 101 101 6 5 3 4 1 5 3 3 0 4 5 4 1 2 3 4 4 1 2 1 3 3 2 99 3 99 99 1 18 2 5 5 3 4 4 3 3 1 2 1)

  ,(Ans 6 5 3 3 4 4 4 2 3 4 3 3 4 4 1 2 3 1 2 2 3 2 1 4 4 1 0 0 0 0 2 1 1 0 1 0 1 0 0 0 "International Studies" 13 2 2 1 1 1 1 1 0 0 1 3 2 1 2 3 3 1 1 0 0 0 0 0 0 4 2 3 2 2 4 2 1 3 2 2 3 20 45 6 0 1 4 3 3 1 5 2 4 0 4 1 2 2 4 3 1 2 1 2 2 2 3 4 2 2 0 18 1 3 3 2 3 3 2 3 1 2 1)

  ,(Ans 6 6 4 4 4 4 4 3 4 4 3 4 3 4 4 4 4 3 4 4 4 3 3 3 4 1 1 0 0 0 4 1 0 7 0 0 1 0 0 0 "Business Administration" 13 2 2 1 0 0 0 0 0 0 1 4 1 3 1 1 1 1 1 0 0 0 0 0 0 1 1 1 2 2 2 101 101 101 101 101 101 5 30 6 3 3 3 3 3 2 4 2 2 2 4 3 2 2 3 3 3 3 2 2 3 3 4 4 4 4 1 18 1 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 6 4 1 4 4 3 2 3 4 3 1 3 3 3 1 2 2 3 3 4 3 1 3 2 1 0 0 0 0 4 1 1 4 0 0 0 1 0 0 "Theater Arts" 13 2 99 1 0 0 0 0 0 0 1 3 3 1 2 2 2 3 1 0 0 0 0 1 0 4 3 3 2 3 4 4 3 4 3 4 4 101 101 6 5 5 3 5 5 5 5 4 5 5 5 3 4 2 5 5 5 4 3 3 4 3 3 4 3 5 1 18 2 4 5 3 4 2 3 3 1 0 1)

  ,(Ans 6 5 3 1 4 4 3 2 3 3 4 3 4 2 2 1 3 1 2 4 3 1 3 3 1 1 0 0 0 0 4 1 1 0 0 1 1 0 0 0 "Film" 10 2 2 1 0 1 1 1 0 0 1 1 3 1 1 2 2 3 0 0 0 0 0 0 1 4 3 4 3 2 3 3 2 4 2 1 3 101 101 5 4 3 4 3 4 2 2 3 1 3 4 3 4 4 5 5 5 3 4 5 2 3 3 5 3 3 0 18 2 5 3 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 2 1 3 4 2 4 3 3 1 3 1 3 1 2 1 1 4 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Foreign Languages" 14 2 99 1 0 1 0 1 0 0 1 2 3 1 1 2 1 3 0 0 0 0 1 1 0 3 4 4 3 3 4 3 3 4 2 3 4 101 101 6 3 5 3 0 5 3 101 0 2 0 4 4 3 2 4 4 3 3 1 3 3 3 4 4 1 4 1 18 2 5 2 3 4 4 3 3 3 3 3)

  ,(Ans 6 5 4 101 4 4 4 3 4 3 4 3 3 3 3 2 4 2 4 4 4 3 2 3 3 1 1 0 0 0 4 1 1 0 1 0 1 0 0 0 "Business Administration" 16 2 1 1 0 1 1 1 0 0 1 6 3 1 2 3 2 3 1 1 0 0 1 0 0 4 4 3 4 4 4 3 3 2 3 4 3 101 101 6 5 5 5 5 5 5 5 3 3 2 5 5 5 4 5 5 3 5 3 4 4 4 5 5 4 5 1 18 1 2 3 3 4 4 3 3 1 2 1)

  ,(Ans 6 2 4 4 4 4 4 4 3 4 3 3 3 3 3 3 3 2 3 3 3 3 2 4 4 1 1 0 1 0 4 1 1 0 0 0 1 0 0 0 "Psychology" 16 2 2 1 1 1 1 0 0 0 3 3 1 2 3 1 2 1 0 1 0 0 0 0 0 3 3 2 2 2 3 3 2 2 2 2 2 20 20 6 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 5 5 4 4 4 5 4 4 4 5 5 1 18 2 1 1 3 3 2 3 3 1 2 1)

  ,(Ans 6 4 4 2 4 4 4 4 4 4 4 4 1 4 4 3 4 3 4 4 4 3 2 4 4 0 1 0 1 0 5 1 1 0 0 0 0 0 0 0 "Biology" 20 2 2 1 0 1 1 1 0 0 1 3 2 3 101 101 101 1 1 1 0 0 1 0 0 4 3 3 3 4 3 4 3 3 3 4 3 10 20 6 5 4 5 5 5 5 5 5 5 3 2 3 4 3 4 4 3 4 3 4 3 4 5 4 4 5 1 18 1 2 3 101 101 101 101 101 101 101 101)

  ,(Ans 6 2 3 4 4 3 4 4 3 4 3 3 3 4 4 3 3 1 2 2 2 1 2 1 3 0 0 0 1 0 3 0 1 0 1 1 0 0 0 0 "Biology" 16 2 2 1 0 0 0 0 0 0 1 6 101 101 2 101 2 1 0 1 0 0 0 0 0 4 3 3 2 3 3 4 3 3 2 3 3 10 40 5 3 3 1 2 0 3 2 0 1 2 1 3 3 3 4 4 4 2 2 2 2 2 4 3 3 3 1 18 1 2 3 3 4 3 3 3 1 1 1)

  ,(Ans 99 4 3 1 1 4 4 4 3 2 4 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 0 0 0 0 4 1 1 101 0 0 0 0 0 0 "Did not answer" 13 2 101 1 1 1 1 1 0 1 1 2 3 1 1 1 1 3 1 0 0 0 1 0 0 4 4 4 2 4 4 3 3 3 3 3 4 101 101 6 2 3 1 1 5 3 4 0 2 1 5 1 2 2 5 5 3 3 1 2 2 2 5 5 5 5 1 18 1 2 4 3 4 4 3 3 0 2 0)

  ,(Ans 3 4 3 3 4 4 4 4 3 4 3 2 3 3 3 3 3 2 3 3 3 2 2 2 3 1 0 0 0 0 3 1 1 0 1 0 1 1 0 0 "Chemistry" 14 2 99 1 0 1 0 0 0 0 1 1 2 2 2 101 2 1 1 0 0 0 0 0 0 3 3 2 3 2 2 3 3 2 3 2 2 101 101 6 5 5 5 5 5 3 5 5 5 5 5 1 2 3 3 3 1 2 2 2 2 2 4 4 3 4 1 18 1 3 4 3 3 4 3 3 0 0 1)

  ,(Ans 3 6 4 1 3 4 4 2 4 4 4 2 4 3 3 1 4 1 4 3 4 2 2 3 1 1 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 15 2 99 1 0 1 1 1 0 0 1 1 3 101 101 101 2 3 1 0 0 1 0 0 0 4 3 3 4 4 3 4 2 3 3 4 2 101 101 6 5 5 4 4 2 5 4 2 5 5 5 4 4 2 4 4 4 3 2 4 4 4 5 5 5 5 1 18 1 3 4 3 4 4 3 3 1 1 1)

  ,(Ans 3 2 4 1 4 4 4 4 3 4 3 1 2 2 2 3 3 1 3 1 1 2 2 1 1 0 1 0 0 0 3 0 0 0 0 0 0 0 0 1 "Chemistry" 15 2 2 1 0 0 0 0 0 0 1 6 1 2 2 3 3 2 0 0 0 0 1 0 0 4 4 101 2 3 4 3 3 101 2 3 4 3 10 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 3 4 2 2 1 18 1 4 4 3 3 4 3 3 1 0 1)

  ,(Ans 6 5 4 1 1 3 4 4 3 2 3 1 3 4 1 3 3 2 2 2 2 4 2 3 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Music" 15 2 101 1 0 1 1 1 0 0 1 2 1 3 2 2 1 1 1 1 0 0 0 0 0 4 3 3 3 4 4 4 101 3 2 3 3 12 25 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 5 4 5 3 4 4 4 5 5 4 5 1 18 1 3 3 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 1 3 4 4 4 3 4 4 3 3 4 3 3 4 1 4 4 4 3 2 3 2 1 1 0 0 0 3 1 1 0 0 1 0 0 0 0 "Elementary Education" 13 2 1 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 0 0 0 0 0 0 1 4 3 4 3 3 3 4 3 4 3 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 2 3 3 4 4 3 3 3 3 3 3 4 4 4 4 1 18 2 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 5 5 3 2 2 4 4 3 3 4 4 4 4 2 2 1 1 1 1 1 3 1 2 2 2 1 0 0 0 0 5 1 1 0 0 0 1 0 0 0 "Psychology" 13 2 2 1 0 1 1 1 0 0 1 6 1 1 1 2 3 2 0 0 0 0 0 0 0 3 1 3 2 4 3 3 1 3 2 4 3 10 30 6 5 5 5 5 5 5 5 5 5 4 5 4 4 4 4 4 4 3 4 4 3 4 4 4 4 4 1 18 2 2 5 3 4 3 3 3 3 3 3)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 3 3 4 4 2 3 3 4 4 3 3 3 3 4 1 0 0 1 0 4 1 1 4 0 0 1 0 0 0 "Communication Studies" 15 2 1 1 0 1 1 1 0 0 1 3 2 2 2 1 3 1 1 0 0 0 1 1 0 4 4 4 3 4 4 3 4 4 3 3 3 10 60 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 5 5 5 5 3 4 4 5 4 4 5 5 1 18 2 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 3 3 3 4 4 2 4 4 4 4 4 4 4 1 3 4 3 3 3 3 4 4 4 0 0 0 1 0 2 1 1 0 0 0 1 0 0 0 "Business Administration" 12 2 1 1 0 0 0 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 4 4 3 3 3 2 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 1 18 2 4 5 3 3 3 3 3 3 2 3)

  ,(Ans 6 6 4 3 3 4 4 4 4 4 2 4 4 4 4 4 4 2 3 3 3 4 2 3 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 12 2 2 1 0 1 1 0 0 0 1 2 1 3 1 1 3 1 1 1 0 0 0 0 0 4 3 4 3 3 3 3 3 4 3 3 3 30 35 6 5 5 3 3 5 3 3 3 5 5 5 1 3 1 4 5 3 1 2 4 4 4 5 5 3 4 1 18 2 4 2 3 3 3 3 3 3 3 3)

  ,(Ans 3 6 4 4 4 4 101 2 3 4 4 4 1 4 1 1 4 2 4 1 4 3 1 4 4 1 0 0 0 0 3 0 1 7 0 0 1 1 0 0 "Business Administration" 15 2 99 1 0 0 0 0 0 0 1 2 3 1 1 1 3 3 1 0 0 1 0 0 0 3 4 3 2 4 3 3 4 3 2 4 3 101 101 6 5 5 5 1 2 4 5 2 5 0 5 3 3 2 2 3 5 3 5 5 2 4 3 3 3 3 0 18 2 5 2 2 4 4 2 3 3 2 3)

  ,(Ans 6 3 3 1 2 4 4 4 3 4 4 4 3 3 1 1 2 1 3 3 3 2 1 4 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "" 13 2 2 1 1 1 0 0 0 0 1 6 3 1 1 1 1 3 0 1 0 0 1 0 0 4 4 4 4 4 3 101 101 101 101 101 101 101 101 101 3 5 2 3 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 5 5 5 3 1 18 2 2 101 3 2 4 3 3 3 0 3)

  ,(Ans 6 4 3 2 4 4 4 4 3 4 4 4 4 4 3 1 3 1 4 3 3 2 3 4 4 1 0 0 0 0 3 1 0 7 0 0 1 0 0 0 "Undecided" 16 2 3 1 0 1 0 1 0 0 1 3 3 1 1 1 1 3 1 1 0 0 0 1 1 3 2 2 1 1 2 3 1 1 1 1 1 101 101 6 5 5 5 5 5 0 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 1 18 2 3 3 3 3 3 3 3 3 3 3)

  ,(Ans 5 2 4 3 4 4 4 4 2 4 4 2 4 3 4 1 3 1 2 4 4 4 1 4 2 1 0 1 0 0 3 1 1 7 0 0 0 0 0 1 "Accounting" 15 2 2 1 0 1 1 1 0 0 1 6 3 1 2 2 2 3 1 1 0 0 1 0 1 4 4 4 3 4 4 4 4 4 2 4 4 101 101 6 5 5 5 4 5 5 5 5 5 5 4 2 2 5 5 5 5 3 3 5 5 5 5 5 5 5 1 18 1 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 2 4 101 101 4 4 4 1 1 4 2 4 3 1 1 3 1 4 3 3 2 3 3 4 1 0 0 0 1 4 1 1 0 0 0 0 0 1 1 "Chemistry" 17 2 101 1 0 0 0 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 1 0 1 3 2 3 1 3 3 3 3 2 1 3 4 101 101 6 5 5 5 4 5 5 4 4 3 5 5 3 2 4 4 5 4 5 3 4 3 4 4 5 3 4 1 18 2 4 3 3 4 4 3 3 3 3 3)

  ,(Ans 5 5 3 3 1 4 4 4 2 3 3 3 3 4 1 4 3 1 3 3 3 3 3 3 1 1 0 0 0 1 4 1 1 0 0 0 0 0 0 1 "Architecture" 13 2 2 1 0 1 1 1 0 0 1 1 1 2 3 101 2 1 0 0 0 0 0 0 0 4 3 3 2 3 4 3 3 3 2 3 4 8 15 6 2 4 5 3 3 2 5 5 5 5 5 3 4 3 4 5 4 3 4 4 3 4 4 4 4 4 1 18 1 2 3 3 3 3 2 2 3 0 3)

  ,(Ans 3 5 3 1 4 4 4 3 3 4 4 3 4 4 3 1 4 1 3 3 4 4 1 4 1 1 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Art" 10 2 1 1 1 1 1 1 0 0 1 1 3 1 1 2 2 3 1 1 0 0 0 0 1 101 101 101 101 101 101 4 4 4 3 2 2 101 101 6 3 5 2 2 3 2 3 3 3 3 5 3 3 2 3 4 3 3 2 3 2 3 4 4 3 3 1 18 2 3 3 3 3 3 3 3 3 2 3)

  ,(Ans 5 3 4 4 4 3 4 4 4 4 4 4 2 2 2 4 3 2 3 3 3 3 3 3 4 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Business Administration" 13 2 2 1 0 1 1 0 0 0 101 3 2 1 3 2 1 1 1 1 0 0 0 0 0 101 3 101 3 101 101 3 101 4 101 4 3 12 30 6 4 5 5 5 5 4 5 2 4 1 4 2 2 3 3 5 4 3 3 3 3 3 1 1 1 1 1 18 2 1 2 3 4 3 3 3 3 2 3)

  ,(Ans 3 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 3 1 3 2 2 2 2 3 2 1 0 0 0 0 3 1 1 0 0 0 1 1 0 0 "Undecided" 12 2 2 1 0 0 0 0 0 0 1 1 101 2 2 101 2 2 1 0 0 0 0 0 0 101 101 101 101 101 101 3 3 3 2 4 4 50 60 6 3 5 4 4 4 4 3 2 3 2 4 1 3 1 3 3 4 3 3 2 2 2 3 4 3 4 1 18 2 3 5 3 2 2 3 3 0 0 3)

  ,(Ans 6 3 4 2 3 4 3 4 4 3 3 4 4 4 3 4 3 2 3 4 3 3 2 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Architecture" 15 2 2 1 0 1 1 0 0 0 1 6 2 2 2 2 3 1 1 0 0 0 0 1 0 2 2 3 2 4 3 2 2 3 2 4 3 20 45 6 5 5 5 5 5 3 5 5 5 5 5 5 4 4 4 4 3 3 4 5 4 4 5 5 4 4 1 18 1 4 4 3 1 2 3 3 3 0 3)

  ,(Ans 6 6 3 1 4 4 3 4 2 4 4 3 4 2 1 1 2 1 1 3 2 2 1 4 2 1 0 0 0 0 3 0 1 0 0 0 1 1 0 0 "Undecided" 14 2 2 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 0 0 1 4 3 3 3 3 4 4 2 2 3 2 4 101 101 6 5 5 5 5 3 1 4 0 5 5 3 4 4 3 5 5 2 3 1 5 5 4 5 4 4 5 1 18 2 5 5 3 3 4 3 3 3 3 3)

  ,(Ans 6 5 4 101 101 4 4 4 4 4 4 3 4 2 2 3 3 1 3 2 2 1 3 3 3 1 0 0 0 0 3 1 1 0 1 1 1 1 0 0 "Undecided" 15 2 101 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 1 1 0 0 0 0 1 4 4 3 2 2 2 101 101 101 101 101 101 101 101 6 5 5 3 3 3 3 4 0 3 0 3 3 3 2 3 4 1 2 2 3 3 3 4 4 3 3 0 18 2 99 2 3 3 4 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 1 2 3 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 4 1 0 7 0 0 0 0 0 1 "Business Administration" 14 2 2 1 0 1 1 1 0 0 1 2 101 1 1 1 1 3 1 0 1 0 0 0 0 4 4 2 2 2 4 4 4 2 2 2 4 101 101 6 4 4 2 1 5 5 5 4 3 5 5 3 3 3 3 3 3 3 3 3 3 3 2 3 1 1 1 18 101 2 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 3 4 4 4 4 4 4 4 2 4 3 4 4 3 3 2 3 2 2 3 2 3 4 1 0 0 0 0 5 1 1 7 0 0 1 0 0 0 "Psychology" 15 2 3 1 0 1 0 1 0 0 101 1 3 2 1 1 3 2 0 0 0 0 1 1 0 3 3 3 3 3 3 101 101 101 101 101 101 10 35 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 18 2 3 2 3 3 3 3 3 1 0 0)

  ,(Ans 6 5 3 2 1 4 4 1 4 2 3 2 2 4 4 2 4 1 4 4 4 1 1 4 4 1 0 0 1 0 4 0 1 0 0 0 0 0 0 0 "Music" 12 2 2 1 0 1 1 1 0 0 1 3 101 101 101 101 3 1 1 0 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 50 55 6 3 5 4 5 5 3 5 3 5 5 5 3 3 3 4 4 3 4 3 4 2 4 3 4 3 3 1 18 2 1 2 3 3 4 3 3 1 0 1)

  ,(Ans 6 4 4 4 4 4 4 4 1 4 4 4 4 3 3 1 4 1 4 4 4 1 1 3 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Biology" 18 2 101 1 0 1 1 1 0 0 1 1 3 1 1 1 3 3 1 1 1 0 0 1 0 4 4 4 4 4 4 4 4 2 2 3 3 101 101 6 4 4 4 5 5 5 5 4 5 5 3 2 3 5 5 5 5 5 3 3 5 5 5 5 5 5 1 18 1 3 4 3 4 4 3 3 1 1 1)

  ,(Ans 5 5 3 1 3 3 3 4 3 4 3 3 4 2 2 1 3 2 2 1 2 1 1 3 2 1 0 0 0 0 2 0 1 0 0 0 0 0 0 1 "Did not answer" 12 2 2 1 0 1 0 0 0 1 1 6 3 1 1 1 2 3 1 0 0 0 1 0 0 4 4 3 2 3 3 3 3 2 2 3 3 101 101 6 5 5 5 3 4 5 5 3 5 5 5 4 3 2 4 4 3 3 3 4 4 4 4 4 4 5 1 18 2 4 5 3 4 4 3 3 1 0 1)

  ,(Ans 5 6 1 1 2 4 4 3 3 2 2 3 2 4 3 4 3 1 4 1 1 1 1 1 1 1 0 0 0 0 3 1 1 99 0 0 0 0 0 1 "Foreign Languages" 15 2 2 1 0 1 1 0 0 0 1 1 2 1 1 1 3 1 0 1 0 0 0 0 0 3 3 3 3 3 4 3 2 2 1 2 4 29 90 99 3 4 3 5 5 5 5 4 4 4 5 4 4 5 4 3 5 4 4 4 3 5 4 4 5 5 1 18 2 4 4 3 4 3 3 3 1 2 1)

  ,(Ans 5 5 2 1 3 3 4 1 4 1 4 4 4 3 3 1 1 1 1 1 1 1 1 4 1 1 0 0 1 0 4 1 0 0 0 0 0 0 0 0 "Anthropology" 20 2 99 1 0 0 1 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 1 0 1 3 2 2 2 3 2 3 1 1 1 3 3 101 101 6 5 5 5 3 1 5 5 3 3 5 2 5 5 4 4 3 4 2 3 4 4 4 5 4 3 3 1 18 2 5 5 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 4 3 4 4 4 2 4 4 2 2 3 3 3 4 3 1 3 1 3 3 3 4 3 1 0 0 0 0 3 1 0 0 0 0 0 0 0 1 "Business Administration" 14 2 99 1 0 1 1 1 0 0 1 3 2 2 2 1 3 1 1 1 0 0 0 0 0 4 3 3 3 4 3 3 3 2 2 3 2 18 30 6 4 3 2 0 2 3 4 0 3 0 5 3 3 2 3 3 3 3 2 2 2 3 3 3 2 3 0 18 2 5 4 3 3 4 3 3 0 0 1)

  ,(Ans 6 5 4 3 2 3 4 3 2 3 3 4 3 2 1 2 1 1 1 1 1 1 1 4 4 1 0 0 0 0 4 1 0 99 0 0 0 0 0 1 "Music" 16 2 2 1 1 1 1 0 0 0 1 2 3 1 1 2 2 3 1 0 1 0 0 0 1 3 3 3 2 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 18 2 4 5 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 1 4 4 4 2 3 4 3 2 3 4 2 1 4 1 4 3 2 3 1 1 2 1 0 0 0 0 5 1 1 0 0 1 0 0 0 0 "Did not answer" 15 2 2 1 0 1 0 0 0 0 1 2 3 1 1 1 2 3 0 0 0 0 1 0 1 4 3 4 2 4 4 3 2 3 2 3 4 101 101 6 5 5 3 1 5 3 5 2 4 2 2 3 3 3 4 4 3 3 3 2 2 3 3 4 3 3 1 18 1 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 3 3 3 3 4 4 4 4 3 3 3 3 4 3 3 1 3 1 3 2 3 2 3 3 4 1 0 0 0 0 3 0 1 0 0 1 1 0 0 0 "Undecided" 14 2 2 1 0 1 0 0 0 0 1 6 3 1 1 2 2 3 1 1 0 0 0 0 1 3 3 4 3 4 2 3 2 3 3 4 2 101 101 6 5 5 5 4 5 5 5 5 5 3 5 4 4 5 5 5 5 3 4 4 4 5 5 4 5 5 1 18 2 2 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 3 4 4 3 3 2 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 4 1 0 0 0 0 1 0 0 0 "Accounting" 14 2 1 1 0 1 1 0 0 0 1 4 2 2 2 101 2 1 0 0 0 0 0 0 0 3 3 101 2 3 3 3 3 101 2 2 3 15 20 6 5 5 5 4 3 4 3 1 3 1 5 3 4 3 3 3 3 3 2 3 3 3 3 3 3 3 0 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 2 2 4 4 4 1 2 1 4 2 1 4 1 2 1 1 3 1 1 0 0 0 0 4 101 1 7 1 1 0 1 0 0 "Undecided" 15 2 2 1 0 1 1 0 0 0 1 1 1 3 1 1 1 1 0 0 0 0 0 0 0 4 3 4 2 3 4 3 2 3 2 2 4 15 30 6 5 5 2 5 5 4 5 3 5 2 5 2 2 2 3 3 4 3 3 101 3 3 5 4 1 3 1 18 2 1 2 3 4 2 3 3 3 3 3)

  ,(Ans 6 3 3 1 4 4 4 4 2 4 4 4 3 4 4 1 2 4 3 3 4 2 3 3 3 0 0 0 1 0 3 1 1 7 0 0 1 0 0 0 "Health Studies" 13 2 99 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 1 0 0 0 0 0 101 101 101 101 101 101 4 3 3 3 4 3 101 101 6 5 5 5 5 4 3 5 5 5 4 5 4 4 3 5 5 3 5 3 4 5 5 5 5 4 5 1 18 2 2 2 3 3 4 3 3 0 0 0)

  ,(Ans 5 4 3 3 4 4 4 4 2 4 4 2 3 4 4 1 4 1 4 4 3 2 1 4 3 1 1 0 0 0 3 1 1 7 0 0 0 0 0 1 "Business Administration" 14 2 2 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 3 2 4 4 4 4 3 1 4 101 101 6 5 5 5 5 5 5 5 3 5 2 5 3 3 3 3 3 4 4 3 3 2 4 3 3 3 4 1 18 2 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 3 5 3 1 4 3 2 1 2 4 3 3 3 2 3 1 3 2 3 2 1 2 2 3 2 1 0 0 0 0 4 1 0 1 0 0 1 0 0 0 "Engineering" 12 2 1 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 0 0 0 0 1 0 0 3 4 3 2 1 4 2 4 3 1 1 4 101 101 6 0 5 4 5 5 5 4 0 5 2 5 1 3 2 4 4 4 2 1 3 2 3 4 4 1 2 1 18 1 2 4 3 4 4 3 3 3 0 3)

  ,(Ans 3 4 2 1 4 4 4 4 1 2 3 2 1 2 3 4 2 1 2 1 3 1 1 1 4 1 0 0 0 0 4 1 1 7 0 1 0 0 0 0 "History" 17 2 2 1 0 0 0 0 0 0 1 6 101 2 101 101 3 2 0 0 0 0 0 0 0 3 3 4 2 4 3 3 2 4 2 4 3 10 65 6 5 5 5 5 5 4 4 1 3 5 2 3 3 4 5 5 3 3 3 3 3 4 5 5 4 4 1 18 1 4 1 1 4 4 3 3 3 0 3)

  ,(Ans 6 4 2 4 4 4 4 4 3 4 4 2 4 3 3 4 2 3 3 3 3 4 2 3 3 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "Biology" 14 2 2 1 0 1 1 1 0 0 1 1 101 3 101 101 101 1 1 0 0 0 0 0 0 101 101 101 101 101 101 3 3 3 4 3 3 15 60 6 5 5 5 5 5 5 5 5 5 5 5 2 2 101 1 2 2 2 2 2 2 2 4 3 3 3 0 18 1 3 3 3 2 2 2 2 0 0 0)

  ,(Ans 3 6 2 1 3 3 3 4 4 4 3 4 4 2 2 4 2 1 2 1 1 1 1 2 2 1 0 0 0 0 3 0 1 7 0 0 0 0 0 1 "Architecture" 16 2 2 1 0 0 0 0 0 0 1 2 101 2 101 101 3 1 0 0 0 1 0 0 0 2 2 2 2 2 2 2 2 2 2 2 3 25 40 6 5 3 3 1 3 5 5 0 1 0 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 1 2 2 3 4 4 3 3 0 0 1)

  ,(Ans 3 6 4 4 4 4 4 4 4 4 4 3 3 4 4 2 2 1 3 3 4 1 2 3 4 1 1 0 0 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 17 2 1 1 0 0 0 0 0 0 1 6 1 1 2 1 3 1 1 0 0 0 1 0 0 4 4 4 4 4 4 4 4 4 3 101 4 12 60 6 5 5 5 4 5 5 5 3 5 5 5 3 2 4 5 5 4 5 3 4 4 4 5 4 2 1 1 18 1 2 1 3 4 4 3 3 1 0 1)

  ,(Ans 6 4 3 2 3 4 4 4 2 3 2 3 2 4 2 1 2 1 2 2 2 3 2 3 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Physics" 14 2 2 1 0 1 1 1 0 0 1 1 101 3 2 101 2 2 1 1 0 0 0 0 0 3 3 3 3 2 3 3 2 2 2 1 3 15 20 6 3 5 4 3 4 4 4 2 5 1 5 3 3 2 3 3 3 3 2 4 2 3 3 4 4 3 1 18 2 2 5 3 4 4 3 2 1 2 1)

  ,(Ans 6 6 4 1 4 4 3 2 1 4 3 2 3 4 4 4 3 1 3 2 3 1 2 2 2 1 1 0 0 0 3 1 1 0 0 0 0 0 1 0 "Computer Engineering" 18 2 2 1 0 1 1 1 0 0 1 1 101 2 101 101 3 1 0 0 0 0 0 0 0 4 4 2 3 2 1 4 4 2 3 1 1 20 100 6 2 5 3 0 1 4 5 0 1 0 0 2 2 2 3 3 4 3 1 3 2 3 3 4 2 2 0 18 1 3 4 3 4 3 3 3 1 0 1)

  ,(Ans 6 6 4 1 3 2 3 3 3 3 2 4 3 4 3 4 2 1 1 1 1 2 1 1 1 0 0 0 1 0 3 1 1 0 0 0 1 0 0 0 "Administration of Justice" 12 2 101 1 0 1 1 1 0 1 1 1 1 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 2 3 3 4 3 4 1 4 4 2 30 6 4 4 4 5 4 1 3 0 3 0 3 2 3 3 3 3 3 4 1 3 3 3 3 3 3 3 1 18 1 4 4 3 4 4 3 3 1 1 0)

  ,(Ans 6 6 4 1 1 4 4 1 3 1 4 4 4 4 4 1 1 1 1 1 1 1 1 4 1 0 0 0 0 1 5 1 1 4 0 0 0 0 0 1 "Communication Studies" 18 2 99 0 0 0 0 0 0 1 1 6 3 1 1 1 1 3 0 0 0 0 0 0 1 3 4 4 4 101 101 101 101 101 101 3 4 101 101 6 5 5 5 0 3 3 2 1 2 0 5 3 3 3 3 3 5 3 3 3 3 4 3 3 3 3 1 18 2 3 3 3 4 2 3 3 3 2 3)

  ,(Ans 6 4 4 2 3 3 4 3 4 3 3 3 3 3 3 1 1 1 3 3 4 1 1 3 4 1 0 0 0 1 5 0 1 7 1 0 0 0 0 0 "Child and Family Studies" 12 2 101 0 0 1 0 0 0 0 1 6 3 1 1 1 3 3 1 0 0 0 1 1 0 2 4 4 4 4 3 3 4 4 3 3 2 101 101 6 3 3 5 5 3 2 3 1 3 5 2 2 1 2 3 2 3 3 2 2 3 2 4 3 3 3 0 18 2 2 2 3 3 4 2 3 3 2 3)

  ,(Ans 6 6 4 1 4 3 4 1 4 4 1 3 3 4 4 4 2 1 3 1 1 1 2 2 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Business Administration" 13 2 2 1 0 1 0 1 0 0 1 1 2 1 1 1 3 1 0 0 0 0 0 0 0 4 4 3 2 3 4 1 2 1 1 1 2 101 45 6 5 5 5 5 5 5 5 5 5 5 5 3 4 3 5 5 5 4 3 3 2 2 3 3 2 3 1 18 2 99 2 3 4 1 3 3 3 0 3)

  ,(Ans 3 6 3 2 4 4 4 3 4 4 3 3 3 4 4 1 3 2 3 3 3 2 1 4 3 1 0 0 0 0 3 0 1 7 1 1 1 0 0 0 "Business Administration" 15 2 2 1 0 0 0 0 0 0 1 3 3 1 1 2 2 3 0 0 0 0 1 1 0 4 4 4 4 3 3 101 101 101 101 101 101 101 101 99 5 5 4 4 3 3 3 2 3 2 3 3 3 2 4 4 3 2 2 3 3 3 4 4 3 4 1 18 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 6 2 4 3 4 4 3 4 1 2 3 3 2 4 3 1 4 3 4 2 3 3 2 4 3 1 1 0 0 0 4 1 1 7 0 0 1 0 0 0 "Health Studies" 15 2 2 1 0 1 1 1 0 0 1 3 2 2 101 101 2 2 1 1 0 1 0 0 0 4 4 4 4 4 4 4 4 4 4 4 101 30 60 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 18 2 2 3 2 2 2 2 2 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 4 4 4 4 3 4 3 2 1 2 1 4 1 2 1 1 4 4 1 0 0 0 0 4 1 1 0 1 0 1 0 0 0 "Undecided" 13 2 101 1 0 1 1 0 0 0 1 1 3 1 2 1 2 3 1 0 0 0 0 0 0 4 4 3 3 3 3 3 3 1 1 1 2 101 101 6 5 3 5 2 5 4 3 3 5 3 5 3 3 5 4 4 4 3 3 5 4 4 3 4 4 4 1 18 2 3 5 3 3 4 3 3 3 3 3)

  ,(Ans 3 6 4 2 4 4 4 1 4 4 2 3 4 3 4 1 2 1 3 2 1 2 1 2 4 1 0 0 0 0 3 1 1 0 1 1 1 1 0 1 "History" 20 2 99 1 0 1 1 1 0 0 101 3 3 1 1 1 1 3 1 0 0 0 0 0 0 3 2 3 3 3 1 1 3 1 2 3 3 101 101 6 5 5 5 5 5 5 5 4 5 2 5 5 3 4 2 4 2 5 2 4 3 5 3 3 1 1 1 18 2 3 5 3 2 1 3 3 3 3 3)

  ,(Ans 6 5 4 2 2 4 4 2 2 3 3 3 3 4 4 3 3 1 4 2 2 2 1 3 3 1 0 0 0 0 4 1 0 0 1 0 1 0 0 0 "Foreign Languages" 13 2 2 1 0 0 0 0 0 0 1 2 3 1 1 1 3 2 1 0 0 0 0 0 0 4 3 3 4 4 4 4 2 1 3 3 4 101 50 6 5 5 5 5 5 3 5 5 5 3 5 4 4 4 4 4 3 3 3 4 3 4 4 4 4 4 1 18 2 3 4 3 4 4 3 3 3 0 3)

  ,(Ans 3 5 3 2 4 4 4 3 3 4 3 3 3 2 2 3 2 3 3 2 2 3 3 2 2 1 0 0 0 0 3 0 1 3 0 0 1 0 0 0 "Undecided" 13 2 99 1 0 1 1 1 0 0 1 3 3 101 101 2 101 3 0 0 0 1 1 1 0 3 4 3 3 3 3 101 101 101 101 101 101 101 101 6 5 4 3 5 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 1 3 4 3 3 3 3 3 3 0 3)

  ,(Ans 5 2 3 4 4 4 4 4 4 4 4 3 3 3 4 4 4 3 4 4 4 4 4 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 14 2 2 1 0 1 0 0 0 0 1 3 1 3 1 1 1 2 1 1 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 15 55 6 5 5 5 5 5 5 5 5 5 5 5 4 4 3 5 5 4 4 3 4 4 4 3 3 3 3 1 18 2 5 5 3 4 4 2 3 0 0 3)

  ,(Ans 6 5 4 3 4 4 4 3 4 4 1 3 4 4 2 1 3 1 4 4 2 1 1 2 1 1 0 0 0 0 3 1 1 99 0 0 1 1 0 0 "Art" 15 2 99 1 0 1 1 0 0 0 1 6 3 1 1 2 2 3 1 0 0 0 0 0 1 4 4 4 3 4 4 3 3 2 1 4 3 101 101 6 0 5 4 0 5 5 5 4 4 2 5 1 5 3 3 3 5 3 3 4 1 5 3 3 99 99 0 18 2 2 99 3 4 4 3 4 3 2 3)

  ,(Ans 6 6 3 3 4 4 4 2 3 4 4 4 3 4 4 3 2 3 3 3 4 3 4 4 3 1 0 0 0 0 4 0 0 0 1 1 1 0 0 0 "Psychology" 101 2 2 1 0 1 0 0 0 0 3 2 101 3 101 101 101 1 0 0 0 0 1 0 0 3 3 2 3 2 3 3 3 3 3 3 3 14 40 6 3 4 4 5 5 3 4 3 4 4 5 2 3 3 3 3 3 3 2 3 3 3 4 4 4 4 0 18 2 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 3 1 4 4 4 4 3 4 3 3 3 2 2 1 2 1 2 2 2 3 4 3 3 1 0 0 0 0 2 1 1 7 1 0 1 1 0 0 "Art" 12 2 1 1 1 1 0 1 0 0 1 6 3 1 1 1 2 3 0 1 0 0 0 0 0 3 3 2 2 2 3 101 101 101 101 101 101 101 101 6 5 3 2 1 1 2 2 1 2 0 1 2 2 2 3 3 2 2 2 2 2 2 3 4 3 3 1 18 2 2 2 3 3 2 3 3 3 2 3)

  ,(Ans 6 6 4 1 4 3 4 2 4 4 4 2 2 3 3 4 1 1 2 1 1 1 1 1 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Did not answer" 15 2 101 1 0 1 1 1 0 0 1 2 3 101 101 101 3 1 1 0 0 0 0 0 0 101 101 101 101 101 101 3 2 2 2 3 2 15 75 6 5 5 3 4 3 4 3 5 3 5 5 2 3 4 3 3 2 5 3 3 2 3 3 4 3 4 1 18 1 2 2 3 3 2 3 3 3 0 3)

  ,(Ans 6 5 4 1 3 4 3 3 2 2 3 3 3 3 2 1 3 2 3 1 2 1 1 1 1 1 0 0 0 0 5 1 0 0 0 0 0 0 0 1 "Communication Studies" 13 2 2 0 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 1 0 0 0 0 1 1 2 3 3 2 101 101 101 101 101 101 1 101 101 6 5 5 5 5 5 2 3 4 4 4 4 3 3 2 5 5 3 2 2 3 2 3 5 5 5 5 1 18 2 2 5 2 2 2 2 2 3 3 3)

  ,(Ans 6 5 4 4 4 3 101 3 2 4 2 2 1 4 4 1 1 1 3 3 2 3 1 1 3 1 1 0 0 0 4 0 0 0 1 1 1 0 0 0 "Architecture" 13 2 101 0 0 1 0 0 1 0 101 2 3 1 2 1 3 2 1 0 0 0 0 1 0 3 3 2 3 3 3 101 101 101 101 101 101 101 30 6 101 101 101 101 3 0 1 0 101 101 1 1 2 2 2 1 1 1 1 2 1 3 3 99 2 1 0 18 1 4 4 3 4 4 3 2 0 0 0)

  ,(Ans 5 3 3 1 4 4 4 4 2 4 2 3 3 3 3 4 2 2 3 2 3 2 3 2 1 1 0 0 0 0 3 0 1 7 0 1 0 0 0 0 "Psychology" 13 2 2 1 0 0 0 0 0 0 1 1 2 1 1 1 3 1 1 0 0 0 0 0 0 3 3 3 3 4 3 2 3 2 3 4 3 23 100 6 4 2 2 2 2 3 3 2 3 3 4 2 2 2 2 2 3 3 2 2 2 2 3 3 99 99 0 18 1 2 3 3 3 4 2 3 1 2 1)

  ,(Ans 99 3 4 1 1 4 4 4 3 3 3 4 4 2 2 1 2 1 2 2 3 1 3 4 3 0 0 0 1 0 4 1 1 0 0 0 0 0 0 1 "Science" 13 2 1 1 0 1 1 0 0 0 1 6 3 1 1 2 2 3 1 1 0 0 0 0 0 4 4 4 4 4 4 4 4 4 3 4 4 101 101 6 5 101 5 5 101 4 5 4 5 3 5 4 5 5 5 5 4 4 3 5 3 5 5 5 3 3 1 18 1 3 1 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 3 2 4 4 4 3 4 4 4 1 4 4 2 3 4 2 3 2 2 3 3 1 1 1 0 0 0 0 4 1 1 1 0 0 0 0 0 0 "Business Administration" 14 2 2 1 0 0 0 0 0 0 1 1 3 3 101 101 3 2 1 0 0 0 0 0 0 4 4 4 4 1 4 4 3 2 1 1 2 8 40 6 4 4 4 4 4 5 5 4 1 1 1 3 3 3 5 5 5 3 3 3 3 3 3 3 3 3 1 18 1 2 3 3 2 3 3 3 1 1 1)

  ,(Ans 6 6 4 3 4 4 4 4 3 4 4 4 3 4 3 4 3 1 3 3 4 1 1 2 4 1 0 0 0 0 4 0 1 0 0 1 1 0 0 0 "Business Administration" 16 2 99 1 0 1 0 1 0 0 1 3 1 3 1 1 1 1 0 1 0 0 0 0 0 4 4 3 2 2 3 4 4 2 1 3 3 8 30 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 5 5 4 3 4 3 3 4 4 4 4 1 18 1 2 2 3 3 4 3 3 0 2 0)

  ,(Ans 6 6 4 4 4 4 4 2 4 4 4 4 4 4 4 4 3 3 4 3 4 4 4 4 2 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Did not answer" 14 2 2 1 0 1 0 1 0 0 1 1 1 2 2 1 3 1 0 0 0 0 1 0 0 4 4 4 3 4 3 4 3 4 3 3 2 10 50 6 4 4 3 5 4 4 5 4 4 3 3 3 3 2 4 4 4 4 2 3 3 4 4 4 3 5 1 18 1 2 4 3 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 1 3 4 4 2 3 1 3 1 3 2 2 1 2 1 2 1 2 1 1 2 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Undecided" 13 2 2 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 1 1 0 3 3 3 1 2 4 101 101 101 101 101 101 101 101 6 5 5 5 5 5 3 5 2 5 1 5 3 3 4 4 4 3 5 2 4 3 4 3 3 99 99 0 18 2 3 5 3 4 2 3 3 1 1 1)

  ,(Ans 3 5 4 4 4 4 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 4 1 1 101 0 0 0 0 0 0 "Film" 12 2 2 1 0 0 1 0 0 0 1 1 101 101 3 101 101 1 1 0 1 0 0 0 0 4 4 1 1 1 1 4 2 2 2 4 4 40 40 6 5 5 5 1 2 5 5 2 4 5 2 101 101 101 101 101 101 101 101 101 101 101 5 5 5 5 1 18 1 3 99 3 3 3 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 3 4 2 4 3 4 4 4 3 4 3 2 4 3 3 2 1 2 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Architecture" 17 2 101 1 0 1 1 1 0 0 1 2 3 2 2 1 3 2 0 0 0 0 0 0 0 3 3 4 3 4 3 3 2 4 2 4 3 25 60 6 5 5 5 5 5 4 5 3 5 5 4 4 4 4 5 5 3 3 3 4 3 4 4 3 5 5 1 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 5 4 1 4 4 2 2 2 4 4 4 2 4 4 4 3 1 4 2 3 2 2 3 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Computer Engineering" 15 2 2 1 1 0 1 1 0 0 1 3 3 1 1 1 3 1 0 0 0 0 0 0 0 4 3 3 1 3 4 2 1 1 1 1 2 10 45 6 5 5 5 5 5 2 5 3 5 0 5 3 4 3 3 3 4 2 3 3 3 4 3 3 4 3 1 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 1 3 3 1 4 3 4 4 4 4 2 4 3 4 3 101 2 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Did not answer" 18 2 2 1 0 1 0 0 0 0 3 2 1 3 1 1 1 1 1 1 0 0 0 0 0 3 3 3 3 3 4 3 3 3 3 3 4 27 30 6 2 3 2 4 5 4 5 2 2 5 5 2 2 2 5 5 4 4 2 2 2 2 4 5 4 4 1 18 2 2 2 3 3 2 3 3 3 2 3)

  ,(Ans 6 6 4 3 4 4 3 1 3 4 3 4 3 4 3 1 4 1 3 1 2 2 1 3 2 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Did not answer" 16 2 99 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 0 0 3 3 4 2 2 3 2 1 4 2 1 3 101 101 5 2 2 2 0 1 0 1 0 1 0 2 2 3 3 3 3 3 1 1 3 2 3 3 3 3 3 0 18 2 5 3 3 4 3 3 3 3 3 3)

  ,(Ans 3 5 2 3 3 3 4 3 4 3 3 4 1 1 1 3 2 2 2 2 2 2 2 2 2 1 0 0 0 0 5 0 0 2 0 0 0 0 0 1 "Undecided" 13 2 2 1 0 0 0 0 0 0 1 1 1 3 3 1 3 1 1 0 0 0 0 0 0 4 101 4 101 3 3 101 1 101 1 101 101 15 35 6 0 0 3 5 5 5 5 5 5 5 5 3 3 3 3 3 3 5 3 101 5 5 4 4 4 1 1 18 2 5 99 3 3 2 3 3 3 3 3)

  ,(Ans 6 3 4 2 4 4 4 4 4 4 2 3 2 2 2 1 2 1 3 3 3 1 3 3 2 1 1 0 0 0 3 0 101 0 0 0 0 0 0 1 "Sociology" 12 2 1 1 0 0 0 0 0 0 1 6 3 101 101 2 3 2 1 1 0 0 0 0 0 3 1 3 2 3 3 101 101 101 101 101 101 50 140 6 5 5 5 5 5 5 5 5 5 5 5 3 3 4 5 5 5 5 4 4 4 4 3 3 4 4 1 18 2 1 1 3 4 4 3 1 3 0 3)

  ,(Ans 6 3 4 1 3 3 3 3 2 3 3 3 3 3 2 2 3 1 2 2 1 1 1 1 3 1 0 0 1 0 4 0 1 7 0 0 1 0 1 0 "Music" 16 2 2 1 0 1 1 1 0 0 1 3 1 1 1 1 3 1 1 1 0 0 1 1 0 4 3 3 2 3 3 4 2 3 2 3 3 10 30 6 5 5 5 3 5 5 5 2 5 2 5 3 3 3 4 4 5 4 3 3 3 4 4 4 3 4 1 18 1 3 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 3 1 4 4 4 3 1 4 4 4 4 4 4 4 3 2 4 4 4 1 3 1 4 0 0 0 0 0 5 1 1 101 0 0 0 0 0 0 "Accounting" 13 2 2 1 0 1 1 1 0 0 1 3 1 3 2 1 1 1 0 0 0 0 0 0 0 2 4 1 1 4 2 101 101 101 101 101 101 10 25 6 5 5 5 5 5 5 5 5 5 5 5 3 3 4 5 4 4 5 3 4 3 5 4 4 4 5 1 18 1 3 3 3 4 3 3 3 0 0 3)

  ,(Ans 6 5 4 4 4 4 4 3 3 4 4 3 4 4 3 1 4 2 4 3 4 2 1 4 3 1 0 0 0 0 4 0 0 0 0 1 1 1 0 0 "Accounting" 12 2 1 1 0 1 1 1 0 1 1 1 3 1 2 1 3 3 1 1 0 0 0 0 0 4 4 4 3 3 4 4 4 3 2 3 4 101 101 6 5 5 5 2 2 1 4 3 4 4 5 3 4 2 2 2 2 1 2 3 2 4 3 3 4 3 1 18 2 2 2 3 2 3 3 3 3 2 3)

  ,(Ans 6 3 3 1 1 4 2 4 1 2 4 3 4 4 4 1 4 1 4 1 2 1 1 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Music" 15 2 1 1 0 0 0 0 0 0 1 2 3 101 101 101 101 3 1 0 0 0 0 0 0 4 2 3 2 2 2 4 2 3 2 2 2 101 101 6 5 5 2 5 5 1 5 0 3 5 1 3 3 2 4 4 2 3 2 4 4 2 4 2 3 4 1 18 1 3 4 3 4 4 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 3 4 3 2 3 4 4 2 4 2 1 3 1 3 3 3 2 1 4 1 1 1 0 1 0 4 1 0 7 0 0 1 0 0 0 "Music" 12 2 99 1 0 1 1 0 0 0 1 3 1 1 2 2 3 1 1 0 1 0 1 0 0 4 3 3 2 4 4 4 3 3 2 4 4 5 30 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 101 5 5 5 4 4 4 4 1 18 1 2 4 3 3 4 2 3 3 3 3)

  ,(Ans 6 3 3 3 4 3 3 4 2 3 4 4 3 4 101 2 3 4 3 4 4 3 4 3 3 0 1 0 0 0 4 1 1 0 1 0 0 0 0 0 "Film" 16 2 1 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 1 0 1 3 2 2 1 2 2 3 2 1 2 2 101 101 2 2 3 101 4 3 3 4 3 3 4 3 3 3 4 3 2 3 3 4 2 3 3 5 5 4 4 1 18 1 4 2 2 4 2 3 3 3 0 3)

  ,(Ans 3 6 3 4 4 3 2 1 3 4 4 2 4 3 3 1 3 2 2 2 2 2 3 3 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 12 2 2 1 0 1 0 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 1 0 0 3 2 3 2 3 2 3 1 3 1 3 2 101 101 6 5 4 2 3 5 5 5 5 5 4 3 3 3 3 3 4 3 3 4 3 3 3 3 4 3 3 1 18 1 3 4 3 4 2 3 4 3 0 3)

  ,(Ans 6 5 4 4 3 4 3 4 2 3 1 4 2 2 2 3 3 3 4 3 4 3 1 1 1 1 0 0 0 0 1 1 1 0 0 0 0 0 1 0 "Undecided" 16 2 2 1 0 1 1 1 0 0 1 3 101 3 101 101 3 1 1 1 0 0 0 0 0 4 3 4 3 4 3 3 3 3 2 3 2 25 80 6 5 5 4 5 5 5 5 4 5 1 1 3 3 3 5 5 2 5 4 3 4 3 5 5 5 5 1 18 1 3 5 3 101 101 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 2 3 2 3 4 4 4 4 1 2 2 2 3 2 1 1 3 2 1 1 0 0 0 4 0 1 0 0 0 1 0 0 0 "Theater Arts" 16 2 99 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 1 1 0 0 1 0 1 4 2 2 2 4 4 3 2 2 2 4 3 101 101 6 4 5 5 3 5 5 5 5 5 5 5 4 4 4 5 5 3 4 3 4 4 4 4 5 3 4 1 18 1 3 4 3 4 3 2 3 3 2 3)

  ,(Ans 6 5 4 4 4 4 3 4 3 4 3 4 4 2 2 1 3 1 2 2 2 1 1 3 4 0 0 0 1 0 5 1 1 7 0 0 0 0 0 0 "Economics" 17 2 101 1 0 1 1 0 0 0 1 6 3 1 1 1 3 3 1 0 0 0 1 1 1 3 2 2 3 3 3 3 2 2 3 3 3 101 101 6 5 1 5 2 2 5 5 2 5 1 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 2 2 4 3 4 4 4 2 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 3 4 4 4 1 4 2 2 1 2 2 3 1 2 4 3 2 1 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Did not answer" 13 2 2 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 1 0 0 3 2 2 2 3 2 3 2 2 2 3 2 101 101 6 2 1 1 3 2 3 3 1 1 0 0 2 2 1 4 5 2 1 2 2 3 3 4 4 3 4 1 18 1 4 2 3 4 4 3 3 3 2 3)

  ,(Ans 3 2 3 2 4 4 3 4 2 3 2 3 2 1 1 1 2 1 3 2 3 2 1 2 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Political Science" 15 2 101 1 0 1 1 1 0 0 1 2 3 1 1 1 2 3 1 0 1 0 0 0 0 4 3 4 3 3 3 4 3 4 3 4 3 101 101 6 5 5 5 3 5 3 5 5 5 5 5 4 4 4 3 3 4 5 4 4 3 3 1 4 2 3 1 18 1 3 3 3 4 4 3 3 1 0 1)

  ,(Ans 6 2 4 3 3 3 4 4 3 3 3 3 3 3 3 1 3 2 3 3 3 2 1 2 2 1 0 1 0 0 4 1 1 4 1 0 0 0 0 0 "Biology" 13 2 2 1 1 1 1 1 0 0 1 1 2 1 1 2 3 2 1 1 1 0 0 1 1 4 3 3 3 3 3 101 101 101 101 101 101 1 5 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 5 4 3 4 4 4 4 4 5 4 4 4 1 18 1 2 5 3 3 3 3 3 0 2 1)

  ,(Ans 3 5 4 1 4 4 4 2 4 4 4 3 4 4 4 1 3 2 3 3 3 2 2 3 3 1 0 0 0 0 4 1 0 0 0 0 1 0 0 0 "Undecided" 14 2 99 1 0 1 1 1 0 0 1 6 2 2 2 2 2 3 0 0 0 0 0 1 0 3 3 4 3 3 3 3 3 4 2 3 3 101 101 6 5 5 5 3 4 3 5 3 5 5 5 2 2 3 4 4 3 4 2 3 4 3 3 4 3 3 1 18 1 4 4 3 4 3 3 3 1 2 1)

  ,(Ans 5 3 2 2 4 4 3 4 2 3 4 3 4 2 1 1 2 1 3 1 2 2 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Political Science" 16 2 2 1 0 0 0 0 0 0 1 2 2 2 101 2 3 2 1 0 0 0 1 0 0 3 3 3 2 2 3 4 2 4 1 1 1 2 13 6 3 5 5 5 5 5 5 4 5 4 3 4 4 4 4 4 4 3 3 5 2 4 3 5 4 4 1 18 1 4 5 3 4 2 3 3 1 1 1)

  ,(Ans 6 3 3 2 4 3 4 3 1 4 3 1 4 3 3 4 2 1 2 2 2 3 4 2 1 1 0 0 0 0 3 0 0 4 0 0 0 0 0 0 "Chemistry" 15 2 101 1 0 1 0 0 0 0 1 1 3 101 101 101 3 2 1 0 0 0 0 0 0 4 3 2 2 3 4 3 101 101 101 101 101 1 101 3 2 2 101 1 2 0 2 1 101 0 2 1 1 1 2 2 2 2 2 1 4 2 5 4 3 4 0 18 1 4 4 3 3 4 3 2 0 0 0)

  ,(Ans 6 6 4 1 4 2 4 4 3 4 4 1 3 4 1 4 2 2 4 1 3 4 4 1 4 1 0 0 0 1 2 1 1 7 0 0 1 0 0 0 "Architecture" 15 2 2 1 0 1 1 0 0 0 1 3 1 1 2 1 3 1 0 0 0 0 0 1 0 4 3 4 4 4 4 3 4 4 3 4 4 125 45 6 5 5 5 5 5 5 5 5 5 0 5 2 3 3 3 5 5 5 2 5 2 3 3 4 3 4 1 18 1 2 2 3 4 4 3 3 2 2 1)

  ,(Ans 6 6 4 1 4 4 4 3 3 4 3 3 3 4 3 4 3 1 3 3 3 2 1 2 2 1 0 0 0 0 3 1 1 0 1 1 0 0 0 0 "Business Administration" 14 2 2 1 0 1 1 1 0 0 1 2 1 3 1 1 1 1 0 0 0 0 0 0 0 2 3 3 2 2 2 2 3 3 2 2 2 15 25 6 1 2 2 1 2 2 3 3 3 1 2 1 2 2 2 2 2 2 2 2 2 2 3 4 3 4 1 18 1 2 2 3 4 3 3 3 1 0 1)

  ,(Ans 6 3 4 3 4 4 3 4 4 4 4 4 4 4 4 1 4 1 4 1 1 1 1 1 1 1 0 0 0 0 3 1 1 7 1 1 0 0 0 0 "Business Administration" 15 2 3 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 1 3 3 3 1 3 3 3 3 3 1 3 3 101 101 5 3 3 3 5 2 5 5 1 2 2 0 3 3 4 3 3 3 3 3 3 3 3 5 5 5 5 1 18 2 2 2 3 4 3 3 3 3 0 3)

  ,(Ans 5 3 1 1 3 4 4 3 2 2 3 3 3 3 1 1 2 1 3 2 3 3 3 3 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "English" 16 2 2 1 0 1 0 0 0 0 1 1 3 1 1 2 2 3 1 1 0 0 0 1 0 4 2 3 1 2 3 3 2 3 1 1 3 101 101 6 5 5 5 5 5 4 5 4 5 5 2 4 5 5 3 3 3 3 5 5 5 4 4 5 4 4 1 18 2 2 2 3 3 2 3 3 1 2 1)

  ,(Ans 6 5 4 2 3 4 4 2 2 2 3 4 4 3 2 1 2 1 3 2 3 1 1 3 3 1 0 0 0 0 3 1 1 7 0 0 0 0 0 1 "International Studies" 16 2 99 1 0 0 0 0 0 0 1 2 3 1 1 1 3 3 1 1 0 0 0 0 0 4 3 4 2 3 4 3 2 2 1 3 4 101 101 6 4 5 5 5 5 5 5 4 5 5 5 3 5 4 5 5 4 3 3 3 4 4 4 4 3 4 1 18 2 3 3 3 4 3 3 3 3 2 3)

  ,(Ans 6 3 4 2 4 4 4 4 2 3 3 3 3 3 3 3 2 2 3 2 3 4 2 4 4 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "Undecided" 13 2 101 1 0 1 0 1 0 0 1 101 3 101 101 101 101 3 1 0 0 0 1 0 1 4 3 4 3 4 4 3 3 3 3 4 3 101 101 6 5 5 3 4 5 1 3 2 3 2 5 2 3 2 4 4 2 2 2 2 2 2 4 4 4 5 1 18 2 2 4 3 4 4 3 3 1 2 1)

  ,(Ans 6 6 4 1 4 4 4 3 4 4 4 3 4 4 1 3 4 3 4 2 3 1 1 4 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Business Administration" 14 2 101 1 0 1 1 1 0 0 1 1 2 2 101 101 101 3 0 0 0 0 1 0 0 4 4 3 3 3 4 4 2 1 1 2 3 101 101 6 5 5 5 5 5 5 5 5 5 4 5 3 5 4 5 5 4 3 3 5 4 5 4 5 5 5 1 18 2 2 4 3 3 4 3 3 1 2 1)

  ,(Ans 3 5 3 1 4 4 4 2 4 4 4 3 3 3 3 1 4 1 4 3 4 4 2 4 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Undecided" 13 2 1 1 1 1 0 1 0 0 1 6 3 1 2 1 2 3 1 0 0 0 0 0 0 4 3 4 3 4 3 3 3 3 3 4 3 101 101 6 5 5 3 5 5 2 5 4 5 2 5 4 5 4 4 4 2 3 2 4 3 4 3 4 4 99 1 18 2 4 4 3 4 3 3 3 1 0 1)

  ,(Ans 6 3 3 1 4 3 4 4 3 4 4 3 2 3 2 2 2 3 2 2 2 2 2 2 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Civil Engineering" 15 2 2 1 0 1 0 0 0 0 1 1 101 2 101 101 2 1 1 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 101 3 15 30 101 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 18 1 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 2 4 4 4 3 4 4 4 4 3 2 4 4 2 4 3 2 3 3 3 3 1 0 0 0 0 3 1 1 7 1 0 0 0 0 0 "International Studies" 15 2 2 1 0 1 0 0 0 0 1 3 3 101 101 101 3 1 1 0 0 0 0 0 0 4 3 3 3 4 3 4 3 4 3 4 4 101 15 6 5 5 5 3 5 5 5 5 5 2 5 2 2 3 5 5 2 2 2 3 3 2 3 4 99 99 1 18 2 4 5 3 3 2 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 4 4 3 4 4 3 4 4 2 2 1 2 1 3 1 1 1 1 2 4 1 0 0 0 0 3 1 1 0 0 0 1 0 1 0 "International Studies" 15 2 1 1 1 1 1 1 0 0 1 6 3 1 2 1 2 3 0 0 0 0 1 1 0 1 2 1 3 1 1 4 2 4 1 4 3 101 101 6 5 5 3 3 5 5 4 3 5 5 5 5 5 3 4 4 5 3 4 5 5 5 4 5 3 3 0 18 2 4 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 1 2 2 4 4 2 4 2 2 3 3 3 1 2 1 2 2 1 1 1 2 2 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Mechanical Engineering" 14 1 101 1 0 1 1 0 0 0 1 2 3 1 1 2 2 3 1 0 1 0 1 0 0 4 3 3 3 3 3 3 3 3 3 3 3 101 101 6 5 5 4 5 4 4 4 4 4 4 4 3 3 2 4 4 2 2 3 3 4 3 3 4 4 4 1 18 1 3 3 3 3 2 3 3 3 2 3)

  ,(Ans 6 4 4 4 4 4 4 4 4 4 4 3 3 4 3 1 4 4 4 4 4 2 2 4 3 0 0 0 1 0 4 1 1 0 0 0 1 0 0 0 "Health Studies" 101 1 101 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 101 101 101 101 101 101 4 4 3 3 3 2 101 101 6 5 5 4 4 5 4 3 5 5 5 2 4 4 3 5 5 4 4 3 4 4 4 3 3 4 3 1 18 1 2 5 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 2 2 3 4 2 3 4 3 1 1 3 3 1 3 1 1 3 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "International Studies" 20 1 99 1 0 1 0 0 0 0 1 1 3 1 1 2 2 3 0 0 0 0 1 0 0 3 3 3 2 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 2 4 5 5 5 3 5 3 5 5 3 3 3 4 3 4 3 4 5 5 4 5 0 18 1 4 4 3 3 4 3 3 1 0 1)

  ,(Ans 6 5 3 1 1 3 4 3 1 1 4 4 4 4 3 1 4 4 4 3 4 2 3 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Economics" 17 1 101 0 0 1 0 0 0 1 1 3 3 1 1 1 3 3 1 0 0 0 1 0 0 3 4 2 1 3 3 2 4 1 1 3 3 101 101 6 5 5 5 5 5 5 5 1 5 2 5 2 4 3 5 5 5 3 3 4 4 4 4 4 4 4 1 18 2 4 4 3 3 4 3 3 0 0 0)

  ,(Ans 5 4 4 4 4 4 3 4 1 3 2 1 4 2 2 1 4 1 3 2 2 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Undecided" 15 1 99 1 0 1 0 1 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 0 1 4 1 3 1 1 1 2 1 2 1 1 1 101 101 6 5 5 5 5 5 5 5 5 5 1 0 3 3 2 3 2 3 2 3 3 1 3 2 1 1 1 1 18 2 2 2 3 3 4 2 3 0 0 0)

  ,(Ans 6 3 4 1 3 3 4 3 2 4 3 1 4 3 3 1 3 1 2 2 3 2 1 3 3 1 0 0 0 0 4 1 1 7 0 1 1 0 0 0 "Undecided" 15 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 2 3 1 0 0 0 0 0 0 4 4 3 3 3 3 4 3 3 3 2 2 101 101 6 0 2 4 0 2 0 4 1 3 4 2 1 2 4 5 4 2 5 3 4 3 4 4 5 3 5 1 18 2 2 3 3 3 4 3 3 3 0 3)

  ,(Ans 6 5 4 2 4 4 4 1 2 4 4 3 4 4 1 4 3 1 4 2 2 2 1 2 2 1 0 0 0 0 4 1 1 101 0 0 0 0 0 0 "Did not answer" 12 1 101 1 0 1 1 0 0 0 1 2 1 1 1 1 3 1 1 0 0 0 0 0 0 4 3 3 2 3 4 4 2 3 2 3 4 5 45 6 1 5 5 3 5 5 5 3 5 0 2 2 3 3 5 5 4 5 3 3 3 3 3 4 3 4 1 18 1 2 2 3 2 4 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 1 3 1 3 2 1 1 4 4 4 4 2 2 2 2 1 0 0 0 0 2 1 1 0 1 0 0 0 0 0 "Economics" 13 1 101 1 0 0 0 0 0 0 1 2 3 101 101 101 101 3 0 0 0 0 0 0 0 4 4 4 4 4 4 4 3 3 1 1 1 101 101 5 3 3 2 1 4 3 5 2 2 2 1 1 1 1 3 3 2 2 2 2 2 2 4 4 4 4 1 18 1 2 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 3 3 4 2 3 3 4 3 4 3 4 3 2 4 2 3 2 3 2 4 1 0 0 0 0 5 1 1 7 0 0 0 0 0 1 "Biology" 14 1 101 1 0 1 0 0 0 0 1 3 3 101 101 101 3 1 0 1 0 0 0 0 0 3 3 3 4 3 4 4 3 2 4 3 4 3 45 6 5 5 5 5 5 5 5 3 5 2 3 3 4 5 5 4 4 4 3 4 3 4 4 4 3 4 1 18 1 2 2 3 2 3 3 3 0 0 0)

  ,(Ans 3 5 4 1 3 4 3 3 4 2 3 4 1 2 1 4 2 1 2 1 1 3 1 2 1 1 0 0 0 0 2 1 0 0 0 1 1 1 0 0 "English" 15 1 101 1 0 0 1 0 0 0 1 1 3 1 1 1 3 1 1 0 0 0 0 0 0 3 2 3 3 2 3 3 1 3 1 1 3 101 60 6 5 5 5 2 3 4 5 0 5 0 3 3 3 3 2 1 2 3 1 3 1 2 4 3 2 3 1 18 2 2 2 3 4 4 2 2 3 101 3)

  ,(Ans 6 3 101 3 4 3 4 2 1 4 4 1 3 2 2 1 3 4 2 3 2 1 1 3 3 0 0 0 1 1 4 1 1 99 0 0 1 1 0 0 "Computer Science" 16 1 99 1 0 1 1 1 0 0 1 3 3 101 101 2 3 3 0 0 0 0 0 0 0 3 4 3 1 3 3 101 101 101 101 101 101 101 101 6 5 5 5 2 4 5 2 4 2 4 3 4 3 5 4 4 3 5 4 5 4 3 3 4 2 3 1 18 1 3 4 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 3 101 3 1 2 2 1 3 2 4 2 2 1 3 1 1 2 1 1 3 1 1 1 1 0 0 0 0 4 1 0 7 0 0 1 0 0 0 "Chemistry" 15 1 101 1 0 1 0 0 0 0 1 1 1 1 3 1 2 1 1 0 0 0 0 0 0 3 3 3 2 3 3 2 2 2 1 2 2 8 20 6 4 3 2 101 2 3 5 2 2 0 3 3 3 4 3 4 4 4 3 3 3 4 3 3 2 2 0 18 1 3 5 3 4 4 3 3 3 3 3)

  ,(Ans 5 3 3 2 2 4 4 4 4 3 4 4 2 4 4 4 3 2 3 1 3 3 1 3 2 1 0 0 0 0 4 1 0 0 1 0 0 0 0 1 "Computer Science" 101 1 99 1 0 0 0 0 0 0 1 3 1 2 1 1 3 1 1 0 0 0 0 0 0 4 2 3 2 3 3 3 2 2 1 3 2 8 45 6 3 3 1 0 1 1 2 3 2 1 4 2 3 3 2 2 2 1 4 3 3 3 3 4 3 2 0 18 1 4 5 3 2 2 2 2 0 0 3)

  ,(Ans 6 4 3 1 3 4 3 3 2 3 4 4 3 4 2 1 3 1 3 1 2 2 2 2 3 1 0 0 0 0 5 1 0 0 0 0 1 0 1 0 "Computer Science" 17 1 99 1 0 1 1 1 0 1 1 3 3 1 1 2 2 3 1 0 0 0 0 0 0 4 3 4 3 3 3 4 2 3 2 2 3 101 101 6 0 5 5 4 5 5 5 5 5 3 5 1 4 3 4 4 4 4 3 4 3 3 3 4 1 4 0 18 1 5 5 3 3 4 3 3 3 3 3)

  ,(Ans 3 6 3 1 1 3 4 2 3 4 2 3 4 3 2 3 1 1 1 1 1 1 1 2 1 1 0 0 0 0 3 1 1 7 0 0 0 0 1 0 "Mechanical Engineering" 18 1 101 1 0 1 0 0 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 0 0 3 3 2 2 3 1 3 3 2 2 4 1 101 101 6 3 5 5 4 5 3 5 2 4 5 1 2 2 4 5 3 3 5 2 3 3 3 2 3 2 2 0 18 2 5 2 3 1 4 3 3 3 2 3)

  ,(Ans 3 5 4 4 4 4 4 4 3 3 4 4 3 4 4 4 3 3 4 4 4 3 2 4 4 1 0 0 0 0 3 1 0 7 0 0 0 0 1 0 "Civil Engineering" 18 1 101 1 0 1 1 1 0 0 3 6 2 1 2 1 3 1 0 0 0 0 0 0 0 4 4 3 3 4 4 4 4 3 3 4 4 8 29 6 5 5 5 5 5 5 5 4 4 1 5 3 2 2 5 5 5 2 2 2 2 2 3 3 5 5 0 18 1 2 99 2 2 4 2 3 0 0 3)

  ,(Ans 6 1 4 1 3 3 2 4 2 2 1 1 2 4 1 2 2 1 3 1 2 2 1 1 3 1 1 0 0 0 4 1 1 0 0 0 0 0 0 1 "Anthropology" 17 1 101 1 1 1 1 0 0 0 1 6 2 1 2 1 3 1 0 0 0 0 0 1 0 3 2 2 3 2 1 2 2 2 101 2 1 30 90 5 5 5 5 5 5 5 5 5 5 5 5 4 4 3 3 2 3 4 4 4 2 5 3 4 2 4 1 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 2 4 1 2 4 3 4 2 2 3 3 3 4 4 4 3 2 4 2 3 2 3 3 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Biology" 15 1 101 1 0 1 1 1 0 0 1 1 1 2 2 1 3 1 1 1 1 0 0 0 0 4 3 3 4 3 4 4 3 2 3 2 4 10 16 6 5 5 5 3 4 4 5 5 5 5 5 4 5 3 5 5 5 5 5 5 4 4 4 4 3 4 1 18 1 2 5 3 4 4 3 3 3 2 3)

  ,(Ans 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 1 4 1 4 1 2 4 4 4 1 1 0 0 0 0 3 0 0 0 1 0 1 0 1 0 "Business Administration" 15 1 3 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 1 1 1 0 0 0 1 4 4 4 4 4 101 101 101 101 101 101 101 101 101 6 5 4 4 3 5 4 5 4 3 5 4 2 2 3 2 4 3 3 2 3 5 4 5 5 5 5 1 18 2 2 4 3 101 101 3 3 2 2 2)

  ,(Ans 6 2 4 1 4 3 3 4 3 4 3 4 2 4 2 3 101 3 4 1 2 2 1 2 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Biology" 16 1 101 1 0 1 1 1 0 0 1 1 1 2 3 1 1 1 1 0 0 0 0 0 0 4 3 3 2 3 3 3 2 3 2 3 2 20 35 6 2 4 3 5 5 4 4 4 4 5 3 3 3 3 4 5 3 4 3 3 4 3 4 5 4 5 1 18 2 4 5 3 101 3 3 3 3 2 3)

  ,(Ans 6 3 4 1 1 4 3 4 2 3 3 2 3 3 2 1 3 4 3 2 2 1 1 2 4 0 0 0 1 0 4 0 1 0 0 0 1 0 0 1 "Civil Engineering" 4 1 101 1 0 1 1 0 0 0 1 1 3 1 1 2 1 3 0 0 0 0 0 0 0 3 3 3 3 3 4 3 2 2 2 2 3 101 101 6 5 5 5 4 5 5 5 5 4 5 1 2 3 3 3 4 5 3 3 3 3 4 4 4 3 3 1 18 2 4 4 3 4 4 3 4 3 0 3)

  ,(Ans 6 5 4 1 3 4 4 2 1 3 4 3 4 1 2 2 2 1 2 1 1 1 1 1 1 1 0 0 0 0 3 0 1 0 0 0 0 0 0 1 "Did not answer" 15 1 101 1 0 0 0 0 0 0 1 1 3 1 1 2 2 3 1 0 0 0 1 0 0 3 3 3 2 1 4 4 4 2 2 1 4 101 101 6 5 5 4 2 4 4 3 2 5 5 5 3 2 2 2 4 2 5 1 2 3 5 5 5 5 5 1 18 2 5 3 3 4 3 3 3 3 0 3)

  ,(Ans 5 4 2 1 1 4 4 2 1 1 2 1 3 2 1 1 2 1 2 2 2 1 1 1 1 1 0 0 0 0 4 0 1 101 0 0 0 0 0 0 "Philosophy" 10 1 99 1 0 1 1 0 0 0 1 1 3 1 1 2 1 3 1 0 0 0 0 1 0 3 2 3 1 3 4 3 1 2 1 2 4 101 101 6 5 5 5 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 5 5 5 3 4 3 5 1 18 1 5 3 3 4 4 3 3 3 3 3)

  ,(Ans 3 6 3 101 4 3 4 101 101 4 4 4 4 3 3 3 3 4 3 101 101 101 101 101 101 1 0 0 0 0 3 0 1 4 0 0 1 0 0 0 "Undecided" 15 1 101 1 0 0 0 0 0 0 1 1 3 2 1 1 3 1 1 0 0 0 0 0 0 4 3 3 3 4 3 4 2 2 2 4 2 12 45 6 5 5 5 5 5 5 5 5 5 5 5 3 4 3 5 5 5 4 4 4 5 5 4 4 3 3 1 18 1 4 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 5 3 1 2 4 3 1 2 2 3 2 2 1 4 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Undecided" 12 1 101 1 0 1 1 1 0 0 1 1 1 1 1 1 3 2 0 0 0 0 0 0 0 4 2 3 1 3 4 3 1 1 1 2 3 20 40 6 5 101 2 2 2 5 5 3 5 0 5 5 3 3 3 3 3 4 4 4 3 5 4 3 3 3 1 18 99 3 3 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 3 3 3 4 4 4 4 4 2 4 4 4 4 4 2 2 2 3 3 3 2 1 3 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Business Administration" 16 1 101 1 0 0 0 0 0 0 1 2 1 3 1 1 2 1 0 0 0 0 0 0 0 4 3 2 2 3 3 101 101 101 101 101 101 10 20 6 5 5 4 5 4 0 5 4 4 0 5 3 3 3 3 3 2 3 3 2 3 3 4 4 4 4 1 18 2 3 3 3 3 3 3 3 0 0 0)

  ,(Ans 3 5 3 1 4 4 4 1 1 4 3 4 4 1 1 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Undecided" 12 1 101 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 0 1 4 4 4 4 4 4 4 4 2 2 2 3 101 101 6 5 5 5 5 5 3 4 2 3 0 5 2 2 2 3 3 2 1 2 2 1 3 5 5 3 3 1 18 2 2 5 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 4 1 4 4 4 4 3 4 4 2 3 2 2 3 2 1 3 1 2 1 1 3 3 0 1 0 0 0 4 1 0 0 0 0 0 0 0 1 "Biology" 13 1 101 1 0 0 0 0 0 0 1 6 1 1 3 1 1 1 0 0 0 0 0 0 0 4 3 3 2 3 4 3 2 2 1 3 2 15 25 6 5 5 5 5 5 5 2 4 4 3 2 4 4 5 3 4 5 3 4 5 4 5 3 4 2 3 1 18 1 3 4 3 4 4 3 2 1 0 1)

  ,(Ans 3 3 3 1 4 3 4 4 3 4 4 4 2 2 4 1 1 1 2 2 2 3 4 2 4 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Undecided" 12 1 101 1 0 0 1 1 0 0 1 6 3 1 2 1 1 3 0 0 0 0 1 0 0 3 3 2 1 2 4 3 2 2 1 1 4 101 101 6 5 5 3 3 5 5 5 4 5 2 5 3 4 4 4 5 5 4 2 3 2 4 3 3 3 3 1 18 1 5 4 3 4 4 3 3 1 0 1)

  ,(Ans 6 5 4 1 3 4 4 2 3 3 4 1 4 2 2 1 1 1 3 1 1 3 2 1 1 1 0 0 0 0 3 1 1 0 0 1 1 0 0 0 "Undecided" 16 1 101 1 0 1 0 0 0 0 1 1 3 1 1 2 2 3 1 0 0 0 0 1 0 4 3 3 3 4 4 3 2 3 3 4 4 101 101 6 4 3 4 5 5 4 5 5 5 5 5 3 3 3 4 4 4 4 4 4 4 4 99 99 99 99 1 18 1 4 2 3 4 3 3 3 1 2 1)

  ,(Ans 6 2 4 4 2 4 4 4 2 2 3 4 3 4 3 4 4 2 4 4 4 2 2 4 4 0 0 0 1 0 4 1 1 2 0 0 0 0 0 0 "Undecided" 12 1 99 1 0 1 0 0 0 0 1 3 101 101 101 101 3 1 1 1 0 0 0 0 0 4 3 3 4 4 4 3 3 2 3 3 3 101 30 6 5 5 5 5 5 5 5 4 3 5 5 3 3 3 4 4 4 4 3 3 3 3 3 4 4 4 1 18 2 3 101 3 3 3 3 3 3 2 3)

  ,(Ans 6 2 4 1 1 4 4 4 2 4 4 3 4 4 4 1 3 2 3 3 3 1 1 3 4 1 0 0 0 0 3 1 1 7 1 0 1 0 0 0 "Health Studies" 15 1 101 1 0 0 0 0 0 0 1 3 3 1 2 1 3 3 1 0 0 0 0 0 0 3 3 2 2 101 3 3 3 2 2 101 3 101 101 6 4 4 4 5 5 4 4 101 4 3 4 3 2 2 4 4 3 2 3 3 2 3 4 4 4 4 1 18 1 2 3 3 4 4 2 3 0 2 0)

  ,(Ans 5 3 3 1 3 4 4 2 3 4 4 101 101 101 101 101 101 101 101 101 101 101 101 101 101 0 0 0 0 1 2 1 1 7 1 1 1 1 0 0 "Undecided" 14 1 101 1 1 1 1 1 0 1 1 6 1 2 1 1 3 1 1 0 0 1 0 0 0 2 4 1 3 3 2 3 3 4 1 3 4 23 110 6 5 5 5 4 5 5 4 1 4 5 4 3 2 4 3 2 4 1 2 4 5 4 5 4 3 5 1 18 2 3 3 3 4 4 3 2 3 2 3)

  ,(Ans 6 5 4 1 4 4 4 4 2 4 4 4 4 3 2 1 3 1 3 2 4 1 1 3 2 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Biology" 15 1 99 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 0 1 0 0 0 0 0 4 4 4 3 4 4 3 4 2 1 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 3 4 4 3 3 4 3 5 4 4 3 3 1 18 2 4 4 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 3 4 4 4 4 3 3 3 2 3 3 3 3 3 3 2 3 3 3 3 2 2 3 1 0 0 0 0 4 1 1 0 1 1 0 0 0 0 "Undecided" 14 1 99 1 0 0 0 0 0 0 1 1 3 1 1 1 1 2 0 0 0 0 1 0 0 3 3 3 3 3 4 3 3 3 3 3 4 1 10 5 5 5 5 3 4 3 2 3 3 4 4 2 3 3 3 4 3 2 3 3 3 3 3 3 3 3 0 18 1 5 4 3 2 2 3 3 0 0 0)

  ,(Ans 6 3 4 1 1 4 3 3 1 1 3 3 3 3 3 1 3 1 3 2 1 2 2 3 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "English" 15 1 99 1 0 1 0 0 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 0 1 4 3 3 3 3 4 4 2 3 2 2 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 3 3 3 3 5 3 3 3 4 3 3 3 3 1 18 2 4 99 3 3 4 3 3 3 2 3)

  ,(Ans 3 6 3 1 2 1 1 1 3 3 2 1 1 2 1 1 1 4 1 1 2 1 1 1 4 1 0 0 0 0 4 101 1 99 0 0 1 0 0 0 "Geology" 12 1 101 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 3 4 3 2 1 3 2 3 2 1 1 1 101 101 6 5 5 5 5 5 5 2 5 5 4 5 1 1 2 4 5 3 4 3 3 3 3 1 4 1 1 1 18 1 3 3 3 4 4 2 3 0 0 3)

  ,(Ans 6 4 4 4 4 4 4 4 2 3 4 4 4 4 4 1 2 2 4 4 4 2 4 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Did not answer" 13 1 101 1 0 1 0 0 0 0 1 3 1 1 1 1 3 2 0 0 0 0 0 0 0 101 4 4 101 4 4 3 101 101 1 101 101 20 30 6 5 5 3 1 4 4 4 0 5 0 5 2 3 4 5 4 4 4 2 2 3 5 5 5 5 5 1 18 1 1 1 2 4 4 3 3 3 0 3)

  ,(Ans 6 6 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Communication Studies" 13 1 1 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 5 5 5 2 5 5 2 2 4 3 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 101 0 18 1 2 2 3 2 2 3 3 0 0 0)

  ,(Ans 5 5 3 1 1 4 4 2 4 4 3 4 2 4 4 2 3 1 4 4 4 3 2 4 4 1 0 0 0 0 2 1 1 0 0 0 0 0 0 0 "Undecided" 17 1 101 1 0 0 0 0 0 0 1 2 1 2 2 1 3 2 0 0 0 0 0 0 0 3 3 2 3 3 4 3 2 2 2 2 4 5 30 6 3 5 3 0 5 3 2 2 2 1 5 4 4 3 2 2 3 3 3 3 4 3 4 4 3 5 0 18 1 99 5 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 4 2 3 4 4 3 1 3 2 3 3 4 4 2 4 1 4 4 4 4 4 4 4 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Did not answer" 3 1 101 1 0 0 0 0 0 0 1 1 1 1 1 3 2 2 1 0 0 0 0 0 0 4 3 4 2 3 4 4 3 101 2 3 4 5 28 6 3 4 4 0 1 2 2 4 4 5 3 3 3 3 5 5 3 3 3 4 5 4 5 5 5 5 0 18 1 3 5 3 101 3 3 3 3 3 3)

  ,(Ans 6 5 4 1 1 3 3 3 2 3 4 3 3 2 4 1 3 1 3 2 3 2 2 3 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 13 1 99 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 1 1 4 3 4 4 4 3 3 3 3 4 4 3 101 101 6 5 4 3 4 2 2 3 2 2 2 2 2 3 1 3 4 3 3 2 3 3 3 3 4 3 3 1 18 1 4 5 3 3 4 3 3 3 2 3)

  ,(Ans 6 6 4 4 4 4 4 4 4 4 4 1 4 4 4 1 1 4 4 4 4 4 4 4 1 0 0 0 1 1 5 1 1 0 0 0 1 0 0 0 "Communication Studies" 13 1 99 1 0 0 1 1 0 0 1 1 3 101 101 101 101 3 0 0 0 0 0 1 0 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 3 5 3 0 5 5 5 0 4 2 5 2 3 2 3 3 2 3 2 2 2 3 5 5 5 5 1 18 1 4 3 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 1 1 4 4 4 4 4 4 4 3 4 3 3 1 3 3 2 3 3 2 2 4 1 1 0 0 0 3 0 1 0 1 0 1 0 1 0 "Business Administration" 15 1 101 1 0 1 0 0 0 0 1 3 3 1 2 1 1 3 1 1 0 0 1 0 0 3 4 4 4 4 4 3 4 4 4 4 4 101 101 6 5 5 5 3 5 3 5 5 5 2 5 2 3 3 3 3 2 2 2 2 2 2 4 4 4 4 1 18 1 1 1 3 4 4 2 4 0 0 3)

  ,(Ans 6 2 1 1 4 4 4 4 2 4 2 4 4 1 1 1 2 1 3 4 4 1 1 2 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "History" 14 1 101 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 4 4 4 3 3 4 4 4 3 2 3 101 101 4 5 5 5 1 3 2 2 2 5 2 5 4 3 3 2 1 3 1 3 3 3 3 4 4 3 3 1 18 2 3 3 3 3 4 2 2 0 0 0)

  ,(Ans 6 6 4 3 4 4 4 2 2 4 3 3 4 4 3 1 3 2 3 3 2 2 3 3 3 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Computer Science" 15 1 101 1 0 1 0 0 0 0 1 1 3 1 2 2 2 3 0 1 0 0 0 1 1 4 4 4 2 3 4 3 3 3 2 3 3 101 101 99 0 3 3 1 2 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 5 5 4 4 1 18 1 2 5 3 3 2 3 2 3 2 3)

  ,(Ans 5 3 3 1 3 4 3 2 3 4 2 2 1 2 4 1 1 1 1 1 1 2 1 1 1 1 0 0 0 0 3 0 1 99 0 0 0 1 0 0 "Business Administration" 12 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 1 0 0 0 0 0 1 4 1 1 1 1 1 3 1 1 1 1 1 101 101 6 5 5 5 5 5 3 5 5 5 0 5 5 5 4 5 5 3 5 4 5 3 4 4 3 99 99 1 18 1 4 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 3 1 1 4 3 1 3 3 4 2 3 2 2 2 2 1 2 1 1 1 1 2 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "International Studies" 13 1 101 1 0 1 0 0 0 0 1 3 3 1 2 3 2 3 0 0 0 0 0 0 0 4 3 4 3 3 3 4 3 3 2 2 3 101 101 6 5 4 3 0 3 3 3 3 2 4 5 2 2 2 2 2 2 2 2 2 2 2 2 2 99 99 0 18 1 4 4 101 101 101 101 101 101 101 101)

  ,(Ans 5 5 3 1 3 4 4 2 1 3 2 3 4 4 3 1 3 1 3 1 1 1 1 3 1 1 0 0 0 0 4 0 1 4 0 0 0 0 0 0 "English" 101 1 101 1 0 1 1 0 0 0 1 1 3 1 1 1 3 3 1 0 0 0 0 1 0 3 3 3 3 3 3 1 1 1 1 2 3 101 101 6 5 5 4 2 4 2 2 2 5 3 5 3 4 1 1 1 1 1 1 3 1 3 4 4 2 2 1 18 1 5 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 2 4 4 4 4 4 4 4 4 4 1 3 4 1 1 3 1 4 4 3 2 3 4 1 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "International Studies" 16 1 101 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 1 1 0 0 0 0 4 3 1 3 1 1 4 3 2 2 1 1 101 101 6 2 1 4 1 1 3 4 0 2 1 1 2 1 1 4 5 3 2 1 3 3 3 4 5 4 5 1 18 2 5 3 2 2 2 2 2 3 3 3)

  ,(Ans 3 3 3 1 4 4 4 3 2 4 3 3 3 3 2 1 1 1 1 1 2 1 1 2 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 1 "Mechanical Engineering" 15 1 101 1 0 1 0 0 0 0 1 6 3 1 1 1 2 3 0 0 0 0 1 1 1 4 3 3 2 2 4 4 2 2 1 1 4 101 101 6 3 5 4 5 5 5 5 3 5 5 5 2 2 2 2 2 2 2 2 2 2 2 2 5 4 4 1 18 1 5 3 3 3 3 3 3 3 3 3)

  ,(Ans 6 6 4 4 4 3 3 2 2 4 4 3 3 4 4 2 3 4 2 2 3 2 4 3 3 1 0 0 0 0 4 1 1 0 1 1 1 0 0 0 "Art" 13 1 101 1 0 0 0 0 0 0 1 101 3 1 2 2 3 3 1 0 0 1 0 0 0 4 4 3 3 3 3 4 4 3 3 3 3 101 101 6 5 5 3 5 3 4 2 2 3 5 4 1 2 1 3 4 3 4 1 2 3 2 4 3 2 3 1 18 1 3 3 3 4 2 3 2 3 0 3)

  ,(Ans 6 4 4 4 3 4 4 4 4 4 4 1 4 3 3 1 4 2 4 4 4 3 1 4 4 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "English" 16 1 99 0 0 1 0 0 0 1 1 3 3 1 1 3 1 3 1 1 1 0 0 0 1 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 3 3 3 4 4 4 4 4 4 4 4 3 4 5 5 5 5 5 5 5 5 5 5 5 5 5 1 18 2 2 2 3 3 4 3 3 3 0 3)

  ,(Ans 3 5 4 3 4 4 4 3 2 3 4 3 3 3 4 2 3 4 4 3 3 3 4 4 4 0 0 0 1 0 3 1 1 101 1 0 0 0 0 0 "Undecided" 12 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 0 4 4 4 3 4 4 4 3 4 2 4 4 101 101 6 0 5 5 2 5 5 5 0 5 2 5 2 3 1 3 3 3 2 1 3 3 3 4 4 4 4 0 18 1 2 4 3 3 4 3 3 3 0 3)

  ,(Ans 6 5 4 1 4 4 4 4 4 4 4 4 3 3 3 1 2 4 4 2 1 1 1 1 4 0 0 0 1 0 2 0 1 0 0 1 1 0 0 0 "Undecided" 101 1 101 1 0 1 0 0 0 0 1 3 3 101 101 101 101 3 1 0 0 0 0 0 0 4 4 4 4 4 4 4 3 2 2 3 3 101 101 6 4 4 3 5 5 5 3 1 3 2 5 3 3 2 3 3 4 2 1 4 3 4 4 4 3 3 1 18 1 4 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 6 3 3 4 4 4 4 2 3 3 3 2 1 1 4 2 2 2 2 2 1 3 2 3 1 0 0 0 0 3 0 1 3 1 1 1 0 0 0 "Undecided" 14 1 101 1 0 0 0 0 0 0 1 6 1 1 2 1 3 1 1 1 0 0 0 0 0 4 4 3 1 2 4 101 4 3 1 2 4 101 60 6 5 5 3 5 5 5 5 5 5 3 5 3 2 2 4 4 4 3 4 4 2 4 3 4 5 3 1 18 2 4 2 3 4 4 2 3 3 3 3)

  ,(Ans 6 6 4 3 3 4 4 3 3 3 3 4 4 4 4 1 3 1 3 3 2 3 3 3 3 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Art" 12 1 101 1 0 1 1 1 0 0 1 3 3 1 2 1 2 3 0 0 0 0 0 0 0 4 3 3 3 3 3 3 2 3 2 1 1 101 101 6 5 5 5 4 5 5 5 5 5 5 4 4 3 4 4 4 4 2 4 4 4 4 3 4 4 4 1 18 2 5 3 3 4 4 3 3 3 2 3)

  ,(Ans 6 4 4 4 4 4 4 4 4 3 3 2 3 4 4 4 3 2 3 3 4 4 2 3 3 1 1 0 0 0 4 1 1 0 1 0 0 0 0 0 "Undecided" 13 1 101 1 0 1 1 0 0 0 3 6 101 3 101 101 101 1 0 1 0 0 0 0 0 101 101 101 101 101 101 4 4 4 3 3 3 20 30 6 5 5 5 5 4 2 5 1 3 4 4 3 3 2 5 4 2 3 2 3 4 4 4 5 5 5 1 18 2 3 2 3 3 4 3 3 0 0 1)

  ,(Ans 6 4 3 2 1 4 4 4 3 1 3 3 3 4 3 1 2 1 4 3 4 2 2 4 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Biology" 12 1 99 0 0 0 0 0 0 1 1 3 3 1 1 1 2 3 1 0 0 0 0 0 1 4 3 2 3 3 4 3 3 2 2 2 3 101 101 6 4 5 5 3 3 2 2 2 1 2 4 3 3 3 5 4 2 2 3 3 2 3 5 4 4 5 1 18 1 3 5 3 3 3 3 3 1 1 1)

  ,(Ans 6 6 4 1 3 4 4 1 2 3 3 4 2 4 1 1 3 1 4 2 2 1 1 3 4 1 0 0 0 0 4 1 1 0 1 1 1 1 0 0 "Philosophy" 12 1 101 0 0 0 0 0 0 1 1 3 3 1 1 1 2 3 0 0 0 0 0 0 0 4 3 3 2 3 4 4 2 3 2 3 4 101 101 6 5 5 5 5 5 5 5 0 5 5 5 2 2 2 4 4 4 3 4 3 2 3 3 4 4 5 1 18 2 4 4 3 4 4 3 3 1 2 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 3 2 4 4 2 2 3 1 4 4 4 2 2 3 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Foreign Languages" 13 1 99 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 1 3 4 2 3 2 3 3 3 2 2 2 3 101 101 6 5 5 1 3 5 5 5 5 5 5 5 2 3 2 5 5 5 4 4 4 4 4 4 4 4 4 1 18 2 4 4 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 4 1 4 4 3 4 1 4 3 4 3 4 1 1 2 3 2 2 2 1 2 4 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Civil Engineering" 14 1 101 1 0 1 1 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 3 3 3 3 4 3 3 2 2 2 3 101 101 6 5 5 5 5 5 5 4 4 5 0 3 2 3 3 3 3 4 2 3 3 2 3 4 4 3 3 1 18 1 2 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 5 4 1 1 4 4 4 4 4 4 4 2 4 4 1 3 1 4 4 4 2 2 4 4 1 0 0 0 0 5 1 1 0 1 0 1 0 0 0 "Psychology" 14 1 101 1 0 0 0 0 0 0 1 2 3 1 1 2 1 3 0 0 0 0 1 1 1 4 3 3 2 3 3 2 3 3 2 2 3 101 101 6 4 5 5 1 4 4 4 4 4 4 4 2 3 4 4 4 4 3 3 4 3 4 4 4 3 4 0 18 2 3 5 3 3 3 3 3 1 0 1)

  ,(Ans 5 5 3 1 2 4 4 3 3 1 3 3 2 2 2 3 3 1 3 2 2 2 1 2 2 1 0 0 0 0 3 0 1 0 0 0 0 0 0 1 "Undecided" 13 1 99 1 0 1 1 1 0 0 1 1 3 2 101 101 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 2 1 2 1 2 5 105 6 1 2 5 1 3 3 3 2 3 1 2 2 3 3 3 3 3 2 3 3 3 3 3 3 3 3 0 18 1 5 5 3 4 4 3 3 0 0 0)

  ,(Ans 6 6 4 1 1 3 3 3 3 4 2 3 3 3 3 3 3 3 3 3 2 2 2 3 3 1 0 0 1 0 4 0 1 0 0 0 0 0 1 0 "Biology" 14 1 101 1 0 1 1 0 0 0 1 3 2 2 2 1 3 1 0 0 0 0 0 1 0 3 3 3 3 3 3 3 2 2 2 2 3 10 30 6 5 5 5 1 3 5 5 4 5 5 3 3 3 3 3 3 3 2 3 3 3 3 3 3 99 99 1 18 2 3 3 3 4 4 3 2 3 0 3)

  ,(Ans 6 2 3 1 4 4 4 4 3 3 2 2 4 4 2 1 3 1 3 3 3 2 3 3 1 1 0 0 0 0 4 1 0 4 1 1 1 0 0 0 "Biology" 15 1 101 1 0 1 1 1 0 0 1 6 3 2 1 1 2 3 1 1 0 0 1 0 0 4 4 3 3 3 3 3 3 2 2 2 2 101 101 6 5 5 5 5 5 3 4 3 5 3 3 2 2 3 3 4 3 3 2 2 2 2 4 4 3 4 1 18 2 2 2 3 3 3 3 3 3 101 3)

  ,(Ans 6 3 4 3 4 4 3 4 2 4 3 2 3 3 3 2 1 1 1 1 3 2 1 2 3 1 0 0 0 0 4 0 0 7 1 0 1 0 0 0 "Accounting" 16 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 1 3 3 2 2 3 2 3 3 2 2 3 2 101 101 6 5 5 3 2 0 5 5 3 1 2 3 4 4 4 3 5 5 3 4 3 3 3 4 4 4 5 0 18 1 4 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 4 4 3 3 4 2 4 2 4 2 4 1 1 3 3 3 1 1 2 4 1 0 0 0 0 5 1 1 0 0 0 1 0 0 0 "Architecture" 14 1 99 1 0 1 1 1 0 0 1 3 2 1 1 1 3 1 0 0 0 0 0 0 0 4 4 4 4 4 4 3 3 2 3 3 4 10 40 6 5 5 5 5 5 5 5 4 5 4 5 2 3 2 4 5 5 4 3 4 4 4 3 4 3 4 1 18 2 2 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 4 2 4 4 4 3 4 4 4 3 3 4 4 1 4 2 4 4 4 2 1 4 4 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Mathematics" 14 1 101 1 0 1 0 1 0 0 1 6 3 1 2 1 3 3 1 0 0 0 0 0 1 4 4 3 4 4 4 4 3 2 3 4 4 101 101 6 5 3 5 1 5 1 5 0 4 3 5 4 4 4 4 4 3 4 4 4 4 2 4 5 5 5 1 18 2 3 3 3 2 3 3 3 3 0 3)

  ,(Ans 6 5 3 2 4 3 3 3 3 3 4 3 3 3 3 4 3 2 3 3 3 3 4 3 3 1 0 0 0 0 3 0 1 0 0 1 1 0 0 0 "Undecided" 15 1 101 1 0 0 0 0 0 0 1 2 1 1 3 1 3 1 0 0 0 0 1 0 0 3 3 3 3 3 3 3 2 2 2 2 2 16 60 6 3 3 3 3 3 4 3 1 3 2 3 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 1 18 1 2 2 3 1 1 3 2 3 0 0)

  ,(Ans 6 6 4 1 1 4 4 1 4 1 1 4 4 2 1 4 1 1 1 1 1 1 1 1 1 1 0 0 0 0 5 1 1 99 0 0 0 0 0 0 "Applied Linguistics" 13 1 2 1 0 1 1 1 0 0 1 1 1 2 1 1 3 1 0 0 0 0 0 0 0 4 3 4 3 3 4 4 2 3 2 1 3 101 45 6 5 5 5 5 5 5 5 5 5 5 5 4 5 3 5 5 5 5 4 5 2 5 4 5 3 3 0 18 1 2 4 3 4 2 3 3 0 0 0)

  ,(Ans 6 5 3 1 1 4 4 4 1 3 4 3 4 3 1 1 2 1 3 1 1 1 1 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 1 "Theater Arts" 15 1 101 1 0 0 0 0 0 0 1 6 3 101 101 3 3 3 0 0 0 0 0 0 0 2 2 3 2 3 4 101 101 101 101 101 101 101 101 6 3 5 5 5 5 5 5 3 5 5 5 3 4 4 5 5 3 5 3 4 3 4 4 4 3 5 1 18 2 5 5 3 3 4 3 3 3 3 3)

  ,(Ans 6 2 4 4 4 4 4 4 3 4 4 3 4 4 2 1 3 2 3 3 4 2 2 4 3 1 1 0 0 0 4 0 1 0 0 0 0 0 0 1 "Did not answer" 17 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 1 0 0 0 1 0 0 3 4 4 3 3 4 3 3 4 3 3 2 101 101 6 4 5 3 5 5 5 5 4 5 5 5 3 3 4 4 4 3 4 3 4 4 4 5 5 5 5 1 18 2 4 5 3 3 4 3 3 1 2 1)

  ,(Ans 6 6 4 1 4 4 4 3 4 4 4 2 2 4 3 1 2 1 3 1 1 1 1 1 2 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Applied Linguistics" 13 1 101 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 4 2 1 4 3 3 3 2 1 3 101 101 6 5 5 5 5 5 5 5 5 5 5 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 1 18 1 3 3 3 3 3 3 3 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 4 4 4 4 4 2 4 4 3 4 3 3 4 4 0 0 0 1 0 4 0 1 1 1 0 0 0 0 0 "Psychology" 13 1 101 1 0 1 1 0 0 0 1 2 2 1 2 1 3 1 0 0 1 0 0 1 0 101 101 101 101 101 101 4 3 3 3 3 3 25 45 5 4 4 4 5 4 4 4 4 4 5 4 3 3 4 4 3 3 3 3 3 3 3 4 4 4 4 1 18 2 2 2 3 3 3 3 3 1 0 1)

  ,(Ans 6 4 4 1 4 4 4 4 1 2 4 2 3 4 4 1 3 1 4 3 4 1 1 4 1 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "History" 101 1 101 1 0 1 0 0 0 0 1 2 3 1 2 3 3 3 0 0 0 0 0 0 1 4 3 3 4 3 4 101 101 101 101 101 101 101 101 6 0 5 3 5 5 0 2 5 5 5 3 1 2 1 3 4 1 1 4 2 3 2 3 3 1 3 1 18 1 2 3 3 4 4 3 3 1 0 1)

  ,(Ans 6 6 3 1 3 4 4 1 4 4 4 4 1 1 1 1 1 4 2 2 1 4 2 1 3 1 0 0 0 0 3 0 0 0 1 1 1 0 0 0 "Undecided" 12 1 101 1 0 1 0 0 0 0 1 3 3 1 2 1 2 3 1 0 0 0 0 0 0 3 3 3 1 2 2 3 3 3 1 2 2 101 101 6 1 1 1 1 1 1 1 1 1 1 1 3 3 3 2 5 2 2 2 2 3 2 3 3 3 3 1 18 2 3 4 3 3 4 3 3 1 1 1)

  ,(Ans 6 4 4 3 4 4 3 4 1 3 4 1 3 4 1 1 3 1 4 3 3 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Did not answer" 15 1 101 1 0 1 1 1 0 0 1 1 3 2 101 101 1 3 1 1 0 0 0 0 1 4 3 4 2 3 4 4 1 3 1 2 3 101 101 6 3 5 5 5 5 5 5 4 4 2 4 4 4 5 3 3 5 3 4 5 3 5 3 3 3 3 1 18 2 3 5 2 101 101 1 2 1 101 1)

  ,(Ans 6 6 3 1 4 3 2 3 2 4 3 2 3 4 2 4 1 1 3 1 3 1 4 3 3 1 0 0 0 0 4 0 1 2 0 0 0 0 0 1 "Mechanical Engineering" 16 1 101 1 0 0 1 1 0 0 1 6 1 1 2 1 3 1 0 0 0 0 0 0 0 3 3 2 3 4 1 2 1 3 2 4 2 101 50 6 2 5 5 2 3 2 3 1 3 0 3 1 3 3 2 1 3 1 1 2 1 3 3 2 1 3 1 18 1 2 2 3 4 4 3 2 3 0 3)

  ,(Ans 6 5 4 1 1 4 4 3 1 4 3 1 4 4 4 1 4 4 4 4 3 3 2 2 1 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Undecided" 13 1 2 1 1 1 1 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 4 3 4 4 4 4 4 3 4 101 101 6 5 5 5 2 5 3 4 4 3 5 5 3 3 3 5 5 5 3 4 3 3 5 4 4 3 4 1 18 1 2 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 4 4 3 3 1 1 4 4 4 1 1 1 1 3 4 1 1 1 1 1 3 1 0 1 0 0 0 5 0 1 0 0 0 0 0 0 1 "Secondary Education" 12 1 101 1 0 1 1 1 0 0 1 1 3 101 101 101 101 3 1 0 0 0 1 0 0 3 3 4 3 4 1 2 3 3 2 1 1 101 101 6 5 5 4 2 5 5 5 3 5 5 5 4 5 2 5 5 5 5 3 2 2 5 3 5 3 5 1 18 1 3 4 3 4 4 2 3 3 2 3)

  ,(Ans 6 6 4 3 4 4 4 4 1 4 4 1 1 1 1 1 1 2 2 2 2 1 1 1 1 0 0 0 1 0 4 1 1 0 0 0 0 0 0 0 "Canadian Studies" 13 1 101 1 0 1 0 1 0 0 1 2 3 101 101 101 101 3 1 0 0 0 0 0 0 3 3 3 2 2 2 3 3 4 2 2 2 101 101 6 3 5 5 5 4 5 5 5 5 5 5 3 3 3 5 5 4 4 4 5 3 5 4 3 3 4 1 18 1 2 4 3 3 4 3 3 3 2 3)

  ,(Ans 5 4 4 2 4 4 3 4 4 4 3 3 3 4 2 1 2 1 3 1 2 3 3 2 2 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Biology" 14 1 101 1 0 1 1 1 0 0 1 2 1 1 2 1 3 2 0 0 0 0 0 0 0 3 2 3 1 4 2 4 1 3 1 4 3 10 70 6 5 5 3 5 4 2 5 5 5 2 4 3 2 2 4 5 3 3 5 5 2 3 4 3 1 4 0 18 1 2 4 3 4 4 3 3 3 3 3)

  ,(Ans 6 3 4 1 3 4 4 4 3 4 4 2 2 4 2 2 3 2 4 4 4 3 2 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 16 1 99 0 1 0 0 0 0 0 3 1 3 1 1 1 2 3 0 0 0 1 0 0 0 4 4 4 3 3 4 4 4 4 3 3 4 101 101 101 5 4 2 2 4 1 4 1 4 1 5 2 3 2 3 4 3 4 2 3 5 4 4 4 3 4 1 18 1 2 5 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 3 1 1 3 1 3 3 2 1 3 3 3 3 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Political Science" 12 1 101 1 0 1 0 0 0 0 1 1 3 101 101 101 101 3 1 0 0 0 0 0 0 1 1 1 1 1 2 3 1 3 1 2 3 101 101 6 5 5 5 101 5 3 5 101 5 0 5 3 3 4 1 1 3 3 2 4 101 4 5 2 99 99 0 18 2 1 4 3 4 4 2 2 3 101 3)

  ,(Ans 6 6 4 1 4 4 4 1 3 4 3 2 3 4 2 1 3 4 4 4 4 2 1 1 1 0 0 0 1 0 4 0 1 0 0 0 1 0 0 0 "" 13 1 1 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 4 3 3 3 4 3 4 2 3 3 101 101 6 5 5 5 5 5 5 5 3 3 5 5 3 4 3 4 5 3 4 4 3 4 3 4 4 4 5 1 18 1 4 5 3 3 3 3 3 3 2 3)

  ,(Ans 6 5 4 2 4 3 3 2 2 4 2 3 3 3 3 2 2 4 3 2 2 2 4 2 3 0 0 0 1 0 2 0 1 7 0 0 0 0 0 1 "Business Administration" 13 1 99 1 0 1 1 1 0 0 1 3 3 1 1 1 2 3 1 0 0 0 0 0 0 4 4 4 4 4 101 101 101 101 101 101 3 101 101 6 2 2 1 1 2 0 2 0 2 1 4 2 3 2 4 4 3 2 2 3 3 3 3 3 3 3 1 18 2 2 2 3 3 3 2 2 0 0 0)

  ,(Ans 6 6 4 2 3 4 4 2 2 3 4 2 4 2 3 1 3 1 3 3 3 1 1 3 2 1 0 0 0 0 4 1 1 0 1 1 1 0 0 0 "International Studies" 12 1 101 1 0 1 1 0 0 0 1 3 3 1 1 2 2 3 1 0 0 0 0 1 1 4 3 2 2 2 3 3 3 2 2 2 3 101 101 6 4 4 5 2 5 3 4 2 4 2 2 4 3 3 5 5 3 4 3 3 2 3 3 4 3 4 1 18 2 5 5 3 2 3 3 2 1 0 1)

  ,(Ans 3 5 3 1 1 4 3 3 4 1 2 1 2 1 1 3 1 1 1 1 1 3 1 1 1 1 0 0 0 0 3 0 1 3 0 1 0 0 0 0 "Undecided" 15 1 101 1 0 1 0 0 0 0 1 1 1 3 2 1 2 1 1 0 0 0 0 0 0 3 2 3 3 3 4 3 1 2 2 2 4 10 20 6 5 5 5 3 5 3 5 3 5 1 2 4 5 3 2 2 3 3 4 3 2 5 3 3 2 2 1 18 1 5 5 3 4 4 2 4 1 0 1)

  ,(Ans 6 6 4 4 4 4 4 1 4 4 4 4 3 4 4 4 4 4 4 3 4 1 1 3 2 0 0 0 1 1 4 1 1 0 0 0 0 0 0 1 "Health Studies" 13 1 101 1 0 0 0 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 1 0 0 4 4 101 3 4 4 101 101 4 101 101 101 101 101 6 5 5 5 4 5 5 5 5 5 4 5 2 4 3 5 5 2 3 3 4 2 3 5 4 3 5 1 18 2 2 99 3 3 4 3 3 0 0 0)

  ,(Ans 6 5 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 101 3 3 3 3 3 3 1 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "Accounting" 15 1 101 1 0 1 1 0 0 0 1 1 101 101 2 101 2 1 0 0 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 20 30 6 4 4 4 4 4 4 4 4 4 4 4 1 1 2 4 4 4 4 4 4 2 4 5 5 5 5 0 18 1 3 3 101 101 101 101 101 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 1 4 4 4 4 4 4 3 3 3 1 3 1 3 3 1 2 4 1 0 0 0 1 2 1 1 0 0 0 1 0 0 0 "Accounting" 12 1 101 1 0 1 0 0 0 0 1 6 3 1 1 1 2 3 1 0 0 0 0 0 0 4 3 2 101 3 2 4 3 2 2 3 1 101 101 6 3 4 4 2 5 5 4 3 3 2 4 2 4 4 4 4 3 2 3 3 3 3 4 4 99 99 1 18 1 2 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 6 4 4 4 4 4 4 2 3 3 3 4 3 3 1 3 1 1 2 2 2 4 3 4 1 0 0 0 0 4 1 1 7 0 0 0 0 0 0 "Health Studies" 15 1 101 1 0 1 1 1 0 0 1 3 101 101 2 101 3 101 0 0 0 0 0 0 0 101 101 101 101 101 101 101 101 101 101 101 101 101 60 6 5 5 5 4 5 5 5 5 5 5 5 4 3 5 4 5 3 5 5 5 3 5 3 3 101 4 99 18 1 1 1 3 2 2 4 3 0 0 0)

  ,(Ans 6 6 3 3 3 4 4 1 2 2 4 2 2 3 1 1 1 1 1 1 2 1 1 2 1 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Music" 14 1 101 1 0 1 0 1 0 0 1 1 3 2 2 2 2 3 0 1 0 0 0 0 0 4 3 4 3 3 3 4 1 4 1 1 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 18 1 5 4 3 3 4 3 3 1 0 1)

  ,(Ans 6 3 4 1 2 4 4 3 3 3 3 3 1 4 3 4 3 2 3 3 3 2 1 1 1 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Architecture" 14 1 101 1 0 0 0 1 0 0 1 3 2 2 3 1 1 1 0 0 0 0 0 0 0 4 3 3 2 3 4 3 2 2 1 3 3 15 45 6 4 3 2 5 5 4 4 1 2 2 0 3 3 2 5 5 4 4 2 2 3 4 5 5 4 5 1 18 2 4 3 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 3 4 3 3 4 2 3 4 1 1 2 4 4 3 3 2 1 3 3 0 0 0 0 1 4 1 1 0 0 0 1 0 0 0 "Business Administration" 15 1 101 1 0 1 0 0 0 0 1 6 3 101 101 2 2 3 1 0 0 0 1 0 0 4 4 3 3 4 3 3 3 3 2 4 2 101 101 6 4 4 4 4 4 4 4 4 4 4 4 4 4 3 4 4 5 4 4 3 4 4 3 3 3 3 1 18 1 5 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 6 4 1 4 4 4 4 3 4 3 4 2 4 1 3 1 1 3 1 2 3 3 3 1 1 0 0 0 0 3 0 0 0 0 0 1 0 0 0 "Biology" 15 1 101 1 0 1 1 1 0 1 3 1 1 2 3 101 3 1 0 0 0 0 1 0 0 4 4 4 4 4 4 3 4 4 3 1 1 15 60 6 5 5 5 5 5 5 5 5 5 5 3 3 3 3 4 3 4 3 3 3 3 3 5 5 3 4 1 18 1 4 5 3 3 3 3 3 0 2 0)

  ,(Ans 6 5 4 101 4 4 4 4 4 4 4 4 2 4 4 4 4 1 4 4 101 3 2 3 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Political Science" 17 1 99 1 0 0 0 0 0 0 1 1 1 2 2 1 1 1 1 0 0 0 0 0 0 101 4 101 4 4 101 4 101 4 101 101 4 101 25 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 5 5 4 3 5 5 5 5 4 5 5 5 1 18 2 5 5 3 4 4 3 3 0 0 3)

  ,(Ans 6 3 3 1 4 4 4 2 3 4 4 3 1 4 3 1 1 1 2 1 2 1 1 3 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "International Studies" 13 1 101 0 0 0 0 0 0 1 1 3 3 1 1 1 2 3 1 1 0 0 0 0 1 2 1 2 2 1 3 2 1 1 1 1 2 101 101 6 0 0 1 5 2 0 5 0 1 0 0 2 1 2 3 3 1 3 1 2 1 3 3 4 3 2 0 18 2 4 3 3 3 4 2 3 3 2 3)

  ,(Ans 6 4 4 1 1 3 4 4 4 1 3 3 3 4 4 4 2 1 3 1 1 1 1 4 4 1 0 0 0 0 3 1 1 0 0 1 1 0 0 1 "Undecided" 14 1 101 1 0 1 0 0 0 0 1 3 1 1 1 1 3 1 0 0 0 0 1 0 0 4 3 2 3 2 4 3 2 3 1 1 4 10 55 6 5 5 5 0 5 5 5 3 5 3 5 2 1 5 3 2 4 1 2 3 3 5 3 4 4 4 1 18 1 99 2 3 4 4 4 2 0 0 3)

  ,(Ans 6 6 3 3 4 4 3 3 1 1 3 3 4 4 3 1 3 1 3 1 2 1 1 3 3 1 0 0 0 0 3 1 1 0 1 1 1 0 1 0 "Music" 15 1 99 1 0 0 0 0 0 0 1 3 3 101 101 101 2 3 1 0 0 0 0 0 0 4 2 2 1 3 3 4 2 1 1 4 3 101 101 6 4 3 3 5 5 2 3 0 2 3 2 2 3 2 3 3 2 3 1 2 3 3 5 5 5 5 1 18 2 3 4 1 3 4 3 4 3 2 3)

  ,(Ans 6 3 4 1 1 1 1 4 1 1 1 1 1 3 1 1 2 4 2 2 2 1 1 1 1 0 0 0 0 1 3 0 0 0 0 0 1 0 0 0 "Health Studies" 12 1 101 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 1 4 3 1 1 1 101 4 3 1 1 1 101 101 6 5 5 5 5 1 1 2 2 1 1 1 3 3 3 3 3 3 3 3 3 3 3 99 99 99 99 1 18 1 1 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 4 3 3 4 1 4 3 1 3 3 2 1 1 3 3 2 3 2 1 3 3 0 0 0 1 0 4 1 0 0 0 0 0 0 0 1 "Undecided" 13 1 99 1 0 0 1 0 0 0 1 3 3 1 1 2 1 3 0 0 0 0 1 1 0 4 3 3 4 3 2 4 2 3 3 3 1 101 101 6 5 5 5 5 5 5 5 4 5 2 3 4 4 2 4 4 4 3 3 4 3 4 4 5 3 3 1 18 1 2 3 3 3 2 3 3 0 0 1)

  ,(Ans 6 2 4 1 3 3 4 4 4 4 4 4 3 3 4 3 2 2 2 2 2 3 2 2 2 1 0 0 1 0 2 0 1 0 0 1 1 0 1 0 "Child and Family Studies" 101 1 99 0 0 1 1 0 0 0 1 1 1 1 1 1 3 1 1 1 0 0 1 0 0 4 3 4 3 4 101 4 2 3 2 3 101 101 75 6 4 3 4 5 2 3 2 2 2 1 2 1 1 2 3 3 2 2 2 2 2 2 3 4 3 3 0 18 2 1 1 3 4 4 2 2 0 0 0)

  ,(Ans 6 4 4 1 1 4 3 4 2 4 2 1 3 4 1 1 3 4 3 2 2 1 1 1 1 0 0 0 1 0 4 0 1 0 0 0 0 0 0 0 "Economics" 12 1 101 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 4 4 4 4 4 3 4 3 3 3 4 101 101 6 5 3 4 2 3 4 5 5 5 3 4 4 2 3 4 4 3 5 3 3 4 5 3 4 4 4 1 18 1 2 2 3 3 4 3 2 1 0 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 1 3 3 2 2 3 4 4 2 3 3 2 4 4 0 0 0 1 0 4 1 1 0 0 0 0 0 0 0 "" 16 1 101 1 0 1 1 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 4 3 3 3 4 4 4 3 2 2 3 4 101 101 6 5 5 4 5 5 5 5 5 5 3 5 2 3 3 5 5 4 2 4 4 3 5 4 5 3 4 1 18 2 3 1 3 3 4 3 2 1 0 1)

  ,(Ans 101 101 101 4 4 3 3 3 2 3 3 3 3 3 4 2 3 1 2 2 2 2 3 1 2 0 1 0 0 0 3 1 1 3 1 0 0 0 0 0 "Psychology" 13 1 99 0 1 0 1 0 0 0 1 3 3 1 2 1 1 3 0 0 0 0 0 0 0 4 3 3 1 2 3 101 3 3 1 101 3 101 101 6 5 5 5 3 5 4 3 3 1 1 3 3 3 3 4 4 4 4 3 2 1 3 3 3 3 3 1 18 1 2 5 3 4 4 3 3 1 2 1)

  ,(Ans 6 3 4 1 1 101 4 3 4 3 4 1 1 3 1 1 2 4 2 2 1 1 1 1 1 0 0 0 0 1 3 0 0 0 0 0 0 0 0 1 "Communication Studies" 13 1 99 1 0 0 0 0 0 0 1 3 3 101 2 1 2 3 1 0 0 0 1 0 0 3 101 4 2 3 3 101 3 4 2 3 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 4 4 3 4 3 4 4 4 3 3 3 3 1 18 1 2 4 3 4 4 3 3 0 0 1)

  ,(Ans 6 5 4 1 4 4 4 4 1 4 4 1 4 3 1 1 3 4 3 4 2 2 1 3 1 1 0 0 1 0 2 1 1 99 0 0 0 0 0 1 "Undecided" 12 1 99 1 1 1 1 1 0 0 1 3 3 1 1 1 2 3 0 0 0 0 0 0 0 101 3 3 3 101 101 4 101 101 101 3 3 101 101 6 5 5 5 4 5 4 5 4 5 1 5 4 4 4 5 5 4 4 3 4 2 3 4 4 3 3 1 18 2 5 5 3 4 4 3 3 0 0 1)

  ,(Ans 6 6 4 1 4 2 3 1 3 4 4 1 3 2 1 1 1 3 1 1 1 1 1 1 4 0 0 0 1 0 4 1 0 99 0 0 1 0 0 0 "Undecided" 15 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 1 0 0 1 3 1 1 1 1 3 2 4 1 1 1 101 101 6 5 3 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 1 18 1 4 2 3 4 4 3 3 1 1 1)

  ,(Ans 3 5 3 4 4 4 4 4 4 4 2 4 3 3 3 4 3 3 3 3 3 4 3 3 4 0 1 0 0 0 3 1 1 1 0 1 0 0 0 0 "Undecided" 12 1 1 1 0 0 0 0 0 0 1 3 3 1 1 1 2 3 0 1 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 101 101 2 0 2 3 3 2 3 4 4 4 4 4 2 2 3 3 3 3 3 3 3 3 3 5 4 4 3 0 18 1 5 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 2 4 3 3 2 3 3 2 3 3 2 3 3 3 1 2 2 3 2 2 2 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Environmental Sciences and Resources" 12 1 101 1 0 1 1 1 0 0 1 1 2 3 2 1 3 1 0 0 0 0 0 0 0 3 3 4 2 3 4 2 1 3 1 2 4 30 90 6 5 5 5 3 3 3 3 5 3 3 5 3 3 2 5 3 3 3 3 5 3 3 4 4 4 4 1 18 1 2 3 101 101 101 101 101 1 101 101)

  ,(Ans 6 5 4 4 4 4 4 3 1 4 2 4 3 4 2 1 3 1 3 1 1 1 3 1 4 1 0 0 0 0 4 0 1 7 0 0 0 0 0 1 "Computer Science" 13 1 99 1 0 1 1 1 0 0 1 3 2 1 1 1 3 2 0 0 0 0 0 0 0 2 1 1 1 3 4 2 1 1 1 3 2 4 35 6 4 5 5 5 5 5 5 5 4 2 5 4 5 5 3 3 4 4 3 5 3 5 3 3 3 3 1 18 1 2 3 3 3 4 3 3 1 2 1)

  ,(Ans 6 3 4 4 2 4 4 4 4 4 4 3 4 2 3 2 4 4 2 1 2 3 3 2 4 1 0 0 0 0 5 1 1 7 0 1 0 0 0 0 "Undecided" 12 1 101 1 0 0 1 1 0 0 1 3 3 1 1 1 2 3 1 0 0 0 0 0 0 101 101 101 101 101 101 2 3 2 2 2 2 101 101 6 3 3 2 5 3 2 2 3 3 2 3 2 2 2 3 2 3 1 1 2 2 2 5 5 5 5 1 18 1 2 4 3 4 4 3 3 0 0 1)

  ,(Ans 6 2 4 4 1 4 4 4 4 4 4 3 4 4 2 1 4 4 4 1 2 2 1 3 2 0 0 0 1 0 2 0 0 0 0 0 1 0 0 0 "Did not answer" 12 1 101 1 0 1 1 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 4 4 4 3 3 3 4 4 3 1 1 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 1 3 3 3 1 18 2 5 5 3 4 4 3 3 1 0 1)

  ,(Ans 6 3 4 4 1 4 4 4 4 4 4 1 2 2 2 1 3 4 4 1 2 2 1 1 1 0 0 0 1 0 1 0 0 0 0 0 1 0 0 1 "Undecided" 12 1 99 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 3 3 1 3 3 4 3 3 1 3 3 101 101 6 5 5 5 5 101 5 5 5 5 5 5 4 4 3 2 3 3 1 4 4 5 3 2 4 3 4 1 18 2 5 5 3 4 4 3 3 1 0 1)

  ,(Ans 6 6 4 1 1 3 3 2 4 3 4 3 3 4 3 4 4 4 4 4 3 4 2 3 4 1 0 0 1 0 3 0 0 0 0 0 1 0 1 0 "Computer Science" 18 1 99 0 0 1 0 1 0 1 1 1 3 1 1 1 1 3 1 1 0 0 0 0 1 4 101 101 101 101 3 101 3 3 4 3 101 101 101 6 3 5 2 5 5 5 5 5 4 5 4 3 4 3 5 5 4 5 3 3 2 2 3 4 3 3 1 18 2 4 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 1 3 3 3 1 3 4 3 4 4 4 3 1 3 2 3 3 3 2 1 4 4 1 0 0 0 0 3 1 1 7 0 1 0 1 0 0 "Art" 13 1 99 1 0 1 1 0 0 0 1 6 3 1 1 1 2 3 1 0 0 0 0 0 0 4 3 4 3 3 4 3 2 3 2 3 3 101 101 6 2 3 3 1 3 1 3 1 2 1 2 2 4 4 2 3 2 2 3 3 3 4 4 5 4 4 1 18 1 2 2 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 4 1 4 4 4 4 1 4 4 3 4 4 3 4 4 1 4 1 4 4 1 3 1 1 0 0 0 0 3 1 1 0 1 1 0 0 0 0 "Computer Science" 18 1 101 1 0 0 0 0 0 0 1 1 101 3 101 101 101 1 0 0 0 0 1 0 0 4 4 4 4 4 4 4 4 3 2 1 1 8 25 6 2 3 3 0 2 101 5 4 2 0 1 3 3 3 3 3 3 1 3 3 2 4 3 4 3 3 0 18 1 2 2 3 101 101 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 4 4 3 2 4 4 4 3 4 2 1 2 1 2 1 2 1 1 3 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Mechanical Engineering" 16 1 101 1 0 0 0 0 0 0 1 1 3 101 101 3 2 3 0 0 0 0 0 0 1 4 3 3 2 101 3 3 3 2 1 2 3 101 101 6 5 5 4 3 5 4 5 2 4 5 4 3 4 2 2 2 2 3 2 4 2 1 4 3 2 101 1 18 1 99 4 3 4 4 3 3 3 0 3)

  ,(Ans 3 5 4 1 2 3 4 4 1 1 3 1 3 4 1 1 3 3 3 3 1 1 1 1 1 1 0 0 0 0 3 0 0 0 0 0 0 0 0 1 "Undecided" 14 1 101 1 0 1 1 0 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 3 3 4 2 3 2 2 2 3 1 2 2 101 101 6 5 5 5 5 5 5 5 5 5 3 5 4 4 5 5 5 5 3 5 4 3 4 99 4 99 99 1 18 1 2 5 3 2 4 3 3 3 2 2)

  ,(Ans 5 2 2 1 4 4 4 4 1 3 4 2 2 3 2 1 1 1 1 3 4 2 1 4 2 1 1 0 0 1 1 1 0 0 0 0 1 0 1 0 "Anthropology" 18 1 101 1 0 1 1 1 0 0 1 2 3 1 2 1 3 1 1 1 0 0 1 0 0 4 4 4 3 4 4 4 4 4 2 3 3 30 60 4 5 5 5 5 5 4 3 4 4 2 5 4 4 3 3 3 3 4 4 4 3 4 5 5 5 5 1 18 1 2 4 3 2 2 2 3 3 0 3)

  ,(Ans 6 6 4 3 4 4 4 4 3 4 4 4 2 4 4 4 3 1 4 1 3 1 3 2 4 1 0 0 0 0 2 1 1 0 0 1 1 1 1 0 "Computer Science" 16 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 3 1 0 0 0 0 0 0 0 4 3 4 3 3 3 3 3 3 1 3 2 16 75 6 0 2 1 0 3 2 3 0 0 0 1 1 2 3 2 2 3 1 1 1 2 2 2 3 3 3 1 18 1 4 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 4 4 4 4 4 4 3 4 3 3 2 4 3 1 3 2 4 4 4 2 2 2 4 1 0 0 1 0 3 1 1 7 0 0 0 1 0 0 "Black Studies" 14 1 101 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 3 3 3 3 2 2 2 2 2 2 101 101 6 4 4 4 0 3 2 5 5 4 5 4 3 4 4 4 4 3 4 4 3 4 4 4 4 4 4 0 18 2 2 3 3 3 4 3 3 1 1 1)

  ,(Ans 6 5 3 1 1 3 3 3 3 4 4 3 3 3 3 3 3 2 3 2 3 4 3 3 3 1 0 0 0 0 3 1 1 0 0 0 0 1 0 0 "Art" 13 1 101 1 0 0 0 0 0 0 1 1 3 1 1 2 2 3 0 0 0 0 0 0 1 3 2 2 2 2 3 2 2 2 2 2 2 101 101 6 0 2 1 5 3 0 2 0 1 5 2 2 3 2 3 3 2 4 2 3 3 3 3 5 3 4 1 18 1 3 4 3 4 4 3 3 0 2 0)

  ,(Ans 6 5 3 1 1 4 4 2 4 3 4 4 4 4 4 4 4 1 4 2 3 2 3 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Art" 13 1 99 1 0 1 1 1 0 0 1 1 101 101 3 101 2 1 1 1 0 0 0 0 0 3 2 3 3 3 3 3 2 3 3 2 2 8 20 6 3 3 2 3 5 2 5 2 3 5 2 2 3 3 4 4 4 4 3 3 3 4 5 5 5 5 1 18 2 4 4 3 4 4 3 3 0 0 0)

  ,(Ans 6 5 4 3 4 4 4 4 4 4 4 2 4 4 3 1 4 2 4 4 3 2 1 2 2 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Business Administration" 12 1 101 1 1 1 1 1 0 0 1 2 3 1 1 1 1 3 1 1 0 0 0 0 0 4 3 3 2 3 3 4 3 3 2 3 3 101 101 6 5 5 5 5 5 4 4 3 3 2 3 4 4 4 4 4 4 3 3 3 3 4 4 4 4 4 1 18 2 5 2 3 3 3 3 3 1 2 1)

  ,(Ans 6 5 4 1 4 4 3 2 2 2 3 4 4 3 2 2 3 1 3 2 3 1 2 3 4 1 0 0 1 0 3 1 1 0 0 0 0 0 0 0 "Undecided" 14 1 99 1 0 1 1 1 0 0 1 6 3 1 1 2 1 3 1 1 0 0 0 0 1 4 3 4 4 4 4 4 3 3 4 4 4 101 101 6 5 5 4 5 5 5 5 101 4 5 5 2 2 2 4 4 5 1 2 3 4 2 4 4 3 3 1 18 1 2 4 3 4 4 2 3 1 2 1)

  ,(Ans 6 5 4 1 3 3 4 3 4 4 2 3 3 3 3 2 1 1 1 1 1 1 1 1 3 1 0 0 0 0 3 1 1 7 0 0 0 1 0 0 "Undecided" 17 1 99 1 0 0 0 0 0 0 1 2 3 1 1 1 1 3 0 0 0 0 0 0 0 4 2 3 1 2 3 4 1 2 2 2 2 101 101 6 5 5 5 1 3 5 5 5 5 2 5 3 4 4 5 5 4 4 3 3 3 2 3 3 3 3 0 18 2 5 2 3 3 2 3 3 0 0 0)

  ,(Ans 5 3 2 1 3 4 4 4 4 3 4 4 3 2 3 1 3 1 3 1 2 1 1 3 3 1 1 0 0 0 3 1 1 7 1 0 1 0 0 0 "Undecided" 13 1 101 1 0 1 1 1 0 0 1 3 3 101 101 101 101 3 0 1 0 0 0 0 1 3 2 3 2 3 4 3 3 2 1 1 4 101 101 6 4 5 5 5 5 3 5 2 5 5 5 2 3 2 5 4 2 1 2 3 3 4 4 3 2 3 1 18 2 3 3 3 4 4 3 3 3 0 3)

  ,(Ans 6 5 4 1 1 4 4 4 3 2 3 1 3 4 1 1 2 4 2 1 1 1 1 1 3 1 0 0 0 0 3 0 1 0 1 0 0 0 0 0 "Health Studies" 12 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 4 3 2 1 1 3 4 2 3 2 1 3 101 101 6 4 4 4 1 3 3 5 1 3 2 4 1 2 2 3 4 3 2 3 3 3 2 5 3 4 5 1 18 2 2 3 3 3 4 2 3 0 2 3)

  ,(Ans 6 3 4 1 4 4 4 2 3 3 3 3 4 3 2 1 3 1 4 1 2 1 1 1 2 1 0 0 0 0 4 1 1 3 0 0 0 0 1 0 "Theater Arts" 15 1 101 1 0 1 1 1 0 0 1 3 3 101 101 2 101 3 1 0 0 0 0 0 1 4 3 3 2 3 3 3 2 2 1 3 2 101 101 6 5 3 5 5 5 3 5 3 5 1 5 4 5 4 5 5 3 4 4 4 4 4 5 5 5 5 1 18 1 2 4 3 3 2 3 3 3 2 3)

  ,(Ans 6 4 3 1 4 4 3 3 3 3 4 2 4 3 2 1 2 1 2 1 1 1 1 4 1 1 1 0 0 0 4 1 1 99 0 0 0 1 0 0 "History" 15 1 101 1 0 1 1 0 0 0 1 1 3 1 1 1 2 3 1 0 0 0 0 0 0 4 4 4 2 2 4 3 3 4 2 2 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 3 3 3 2 2 4 2 4 2 4 3 3 99 99 99 18 2 4 5 3 4 4 2 2 3 2 3)

  ,(Ans 3 3 2 1 1 3 3 2 3 3 4 2 4 1 2 4 2 1 2 2 1 1 1 1 3 1 0 0 0 0 4 0 1 0 0 1 1 1 0 0 "Social Science" 18 1 101 1 0 1 0 0 0 0 1 2 1 2 1 1 3 1 0 0 0 0 0 0 0 3 2 3 2 3 2 2 2 3 2 3 2 40 60 6 5 5 5 5 5 5 5 5 5 1 5 5 4 3 3 3 4 5 3 4 4 4 4 4 99 99 0 18 1 5 5 3 4 4 2 3 3 2 3)

  ,(Ans 6 6 4 1 3 4 4 2 3 2 3 3 3 3 3 1 3 1 3 2 2 4 3 2 2 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "History" 15 1 101 1 0 0 0 0 0 0 1 3 3 1 1 1 2 3 1 0 0 0 0 0 0 3 3 3 3 3 4 2 2 1 1 3 3 101 101 6 5 5 5 5 5 5 5 5 5 4 4 4 4 3 5 5 5 3 3 4 4 5 4 4 3 3 1 18 2 3 3 3 4 3 3 3 1 1 1)

  ,(Ans 6 5 4 3 4 4 4 4 4 4 4 3 3 4 4 2 3 4 2 4 4 4 4 3 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Did not answer" 13 1 101 1 0 0 1 1 0 0 1 1 3 1 1 3 2 3 0 1 0 0 0 0 0 101 101 101 101 101 101 4 4 4 3 4 4 101 101 6 5 5 5 2 3 5 5 5 5 4 5 2 4 3 5 5 5 2 2 3 4 2 4 5 3 3 1 18 2 5 5 3 3 4 3 3 0 2 1)

  ,(Ans 5 4 2 1 1 4 3 4 3 3 3 2 4 3 4 1 4 101 4 3 2 3 3 3 2 1 0 0 0 0 3 0 1 0 1 1 1 0 0 0 "Psychology" 13 1 2 1 0 1 1 1 0 0 1 1 3 1 2 1 2 3 1 1 0 0 0 0 0 1 1 1 2 2 1 101 101 101 101 101 101 101 101 6 5 5 5 5 5 4 5 5 5 5 5 3 4 3 4 4 4 4 3 4 4 5 4 5 5 5 1 18 2 4 5 3 4 4 3 3 1 1 1)

  ,(Ans 6 6 4 2 4 4 4 2 4 4 4 3 3 4 4 1 2 1 4 2 4 4 3 3 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Did not answer" 15 1 101 1 0 0 0 0 0 0 1 2 3 1 2 1 1 3 0 0 0 0 0 0 0 3 3 3 1 3 4 3 2 2 1 3 4 101 101 6 3 5 3 5 5 2 5 2 3 3 5 3 4 2 5 5 4 4 3 3 4 5 4 4 5 5 1 18 2 2 2 3 3 4 2 2 3 101 3)

  ,(Ans 3 5 4 4 1 4 3 2 1 4 3 1 1 1 1 1 1 4 1 1 1 1 1 1 3 1 0 0 0 0 4 1 1 99 0 0 0 0 0 1 "Undecided" 12 1 99 1 1 1 1 1 0 0 1 3 2 2 2 2 2 3 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 101 101 6 5 4 2 5 3 3 2 3 3 0 3 3 3 3 4 4 3 2 5 5 3 4 4 4 4 4 1 18 1 3 2 3 4 3 3 3 0 101 3)

  ,(Ans 6 3 4 1 4 4 4 4 2 2 4 2 3 3 3 1 2 1 2 2 3 3 2 4 2 0 1 0 1 0 4 1 1 0 0 0 1 0 0 0 "Film" 13 1 101 1 0 0 0 0 0 0 1 6 3 101 101 2 2 3 1 0 0 0 0 0 0 3 3 3 3 3 4 3 3 2 2 3 4 101 101 6 5 5 3 5 5 2 3 2 4 5 5 1 1 1 4 101 2 3 3 4 5 5 4 4 3 3 1 18 1 5 2 3 2 3 3 3 3 0 3)

  ,(Ans 3 6 3 1 3 3 4 3 4 2 4 3 4 3 3 3 4 1 3 3 2 2 3 2 3 1 0 0 0 0 5 1 1 0 0 0 0 0 0 1 "Music" 12 1 101 1 0 0 0 0 0 0 1 2 3 1 1 1 2 3 0 0 0 0 0 0 0 3 2 3 2 2 3 3 3 3 3 3 3 101 101 6 5 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 18 1 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 4 3 3 3 3 4 4 4 3 3 3 3 4 4 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 5 1 1 7 0 1 1 0 0 0 "Psychology" 13 1 99 1 0 1 0 0 0 0 1 3 3 101 101 101 101 3 1 0 1 0 0 0 1 101 101 101 101 101 101 4 4 4 4 4 4 101 101 6 4 4 4 4 4 4 4 4 4 4 101 2 2 2 2 2 2 2 2 2 2 2 4 4 4 101 1 18 2 2 2 3 4 4 3 3 3 3 3)

  ,(Ans 6 6 4 1 4 3 4 1 4 4 4 1 4 3 4 1 1 1 1 1 3 2 1 2 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Business Administration" 13 1 101 1 0 1 0 1 0 0 1 3 3 1 1 1 2 3 1 1 0 0 1 0 0 3 3 3 3 3 3 4 3 2 2 2 3 101 101 6 5 5 5 2 2 3 2 3 2 3 3 4 101 2 101 3 3 3 3 3 3 3 1 1 1 1 1 18 2 4 5 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 1 3 4 4 4 1 4 3 3 3 4 2 1 2 1 2 1 3 1 1 2 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Architecture" 16 1 101 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 3 3 2 4 4 4 3 3 2 1 3 4 101 101 6 0 2 4 2 5 1 5 3 5 3 5 1 2 3 2 2 3 2 2 3 2 4 3 3 3 3 1 18 2 3 2 3 4 4 3 3 0 0 0)

  ,(Ans 6 3 4 1 1 3 3 1 3 4 4 3 4 4 2 1 2 1 3 1 2 4 1 3 4 1 1 0 1 0 3 1 0 0 1 0 1 0 0 0 "Film" 12 1 101 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 0 0 0 101 101 3 101 2 3 3 3 101 3 101 101 101 101 5 2 5 4 3 5 2 5 0 3 0 1 2 3 1 4 3 2 3 1 1 1 2 2 4 1 1 1 18 1 2 1 3 4 3 3 3 0 0 0)

  ,(Ans 6 5 4 1 1 4 4 3 1 2 4 3 4 4 1 1 4 1 4 1 3 1 1 4 1 1 0 0 0 0 3 1 1 7 0 0 0 0 0 0 "Applied Linguistics" 14 1 99 1 0 1 0 0 0 0 1 1 2 1 1 3 2 2 1 1 0 0 0 0 0 4 3 3 3 4 4 4 2 2 1 3 4 2 10 6 5 5 5 5 5 5 5 5 101 3 5 4 3 5 5 5 4 5 3 4 4 4 5 5 4 4 1 18 2 2 2 3 4 4 3 3 3 2 3)

  ,(Ans 6 6 4 1 1 4 4 1 4 4 4 3 4 3 3 2 3 1 3 1 2 1 1 3 1 1 0 0 0 0 4 1 1 7 1 0 1 0 0 0 "Art" 14 1 101 1 0 1 0 0 0 1 1 1 3 1 2 1 2 3 1 1 0 0 0 0 1 2 2 3 3 3 3 3 2 3 2 3 3 101 101 6 5 5 5 5 5 4 4 4 5 2 5 2 2 3 3 3 2 1 2 3 3 3 3 4 3 3 1 18 2 3 2 3 3 4 3 3 3 0 3)

  ,(Ans 6 2 4 3 3 4 4 4 4 3 4 3 4 3 3 4 3 3 4 3 3 4 1 3 1 1 0 0 0 0 4 0 1 0 1 0 0 1 0 0 "Biology" 11 1 2 1 0 1 1 1 0 0 1 1 1 1 1 1 3 1 1 1 0 0 0 1 0 3 3 2 1 3 2 3 3 2 1 3 1 5 20 6 5 5 4 4 1 1 3 4 5 1 2 3 3 3 5 5 4 3 3 3 4 5 5 5 5 5 1 18 2 4 5 3 4 4 3 3 3 2 3)

  ,(Ans 5 6 1 1 4 4 3 1 1 3 2 3 3 4 1 1 3 1 2 1 1 1 2 1 2 1 0 0 1 0 2 1 1 0 1 0 1 0 0 0 "Art" 12 1 101 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 3 2 2 4 3 2 3 2 2 3 101 101 6 5 5 5 5 5 3 5 0 3 3 5 3 3 3 5 5 4 3 2 3 3 3 3 4 3 4 1 18 2 4 3 3 3 4 3 3 3 3 3)

  ,(Ans 6 3 3 2 3 4 4 4 3 4 3 4 2 3 1 3 2 1 3 1 1 1 1 1 4 1 0 0 0 0 2 1 1 0 0 0 0 0 0 1 "International Studies" 17 1 101 1 0 1 1 1 0 0 1 3 2 2 2 1 3 1 1 0 0 0 0 0 0 1 101 101 101 101 101 4 3 3 2 4 4 20 45 6 5 5 5 5 5 5 5 5 5 2 5 5 5 5 5 4 2 5 4 5 3 5 3 3 3 3 1 18 2 3 4 3 4 3 3 3 1 2 1)

  ,(Ans 6 4 4 1 4 4 4 4 4 4 4 1 4 1 1 101 4 1 1 3 3 1 1 1 3 1 1 1 1 0 5 1 1 99 0 0 0 0 0 0 "Music" 15 1 101 1 0 1 0 0 0 0 1 3 3 1 1 1 101 3 1 0 0 0 0 0 0 4 2 3 3 4 4 4 2 3 2 1 2 101 101 6 5 5 4 5 5 1 3 1 5 5 5 5 5 2 5 4 3 5 3 4 5 4 4 3 2 5 1 18 2 2 5 3 3 4 3 3 1 2 1)

  ,(Ans 6 3 101 4 4 4 4 4 1 4 4 1 4 3 1 1 2 2 3 4 4 1 1 4 1 1 0 0 0 0 5 0 0 0 0 1 0 0 1 0 "Business Administration" 18 1 101 1 0 1 1 1 0 0 1 1 3 2 1 1 2 3 0 0 0 0 0 0 1 4 4 4 4 4 4 3 2 2 3 4 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 5 5 5 5 3 3 5 4 4 5 2 5 0 18 2 5 5 3 3 2 2 2 1 1 1)

  ,(Ans 3 6 3 2 4 4 4 4 3 3 4 3 3 3 3 1 3 1 3 3 4 1 1 4 3 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Foreign Languages" 14 1 101 1 0 0 0 0 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 1 4 3 3 3 3 4 3 3 3 3 3 3 101 101 6 5 5 5 5 5 4 5 5 5 5 5 3 3 3 3 3 3 2 3 3 2 3 3 4 3 4 0 18 2 3 99 3 3 3 3 3 1 0 1)

  ,(Ans 6 5 4 1 4 4 3 3 4 3 4 1 4 4 2 1 1 1 1 1 3 1 1 3 3 1 0 0 0 0 5 1 1 7 1 1 1 0 0 0 "Health Studies" 16 1 101 1 1 1 0 0 0 0 1 3 3 101 101 101 101 3 1 1 0 0 0 1 0 4 4 3 2 1 4 3 3 3 2 1 3 101 101 4 4 5 4 5 3 5 5 5 3 1 3 3 4 1 4 4 1 2 2 4 4 3 4 4 2 3 1 18 2 3 3 3 4 4 3 3 3 101 2)

  ,(Ans 6 6 4 1 3 4 4 2 1 3 3 4 4 3 3 1 3 1 3 3 3 1 1 3 1 1 0 0 0 0 3 1 0 0 1 1 1 0 0 0 "Undecided" 13 1 101 1 0 1 0 1 0 0 1 1 3 2 1 1 1 3 1 1 0 0 0 1 1 3 3 4 3 3 3 3 2 4 2 3 3 101 101 6 5 5 5 3 5 3 4 2 5 5 5 3 3 4 3 4 3 2 3 2 4 3 4 5 3 3 1 18 2 2 2 3 3 4 3 3 3 2 3)

  ,(Ans 6 4 3 1 4 4 4 4 3 4 4 3 3 2 1 3 3 1 1 1 1 1 1 1 1 0 0 0 0 1 4 1 0 0 0 0 0 0 0 1 "Did not answer" 21 1 99 1 0 0 0 0 0 0 1 1 1 3 1 1 1 1 1 0 0 0 0 0 0 4 3 4 4 3 3 4 3 4 4 3 3 30 90 6 5 5 101 5 5 5 5 5 5 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 1 18 2 2 3 3 3 4 3 3 3 3 3)

  ,(Ans 6 6 3 1 4 4 4 2 2 4 4 2 1 1 1 1 3 3 1 1 1 1 1 1 1 1 0 0 1 0 3 1 1 0 1 1 1 0 0 0 "Economics" 12 1 101 1 1 1 1 1 0 0 1 1 3 101 2 101 2 3 1 0 0 0 0 0 1 4 3 2 2 3 4 3 4 3 1 3 3 101 101 6 5 5 5 5 5 0 5 0 5 3 0 2 2 2 2 2 2 2 1 2 2 2 4 4 4 4 1 18 1 3 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 4 1 1 4 4 3 1 3 3 4 4 3 2 1 3 4 3 3 3 1 1 2 1 0 0 0 1 0 4 1 1 0 0 0 0 0 0 1 "Economics" 13 1 101 1 0 0 0 0 0 0 1 3 3 3 1 1 1 3 0 0 0 0 0 0 0 4 2 2 1 1 2 4 2 2 1 1 4 101 101 6 5 5 5 5 5 5 5 3 5 5 5 3 3 3 3 4 4 3 4 4 4 4 5 4 5 5 1 18 2 4 4 3 4 4 3 3 3 0 3)

  ,(Ans 6 5 3 1 1 4 4 3 3 1 4 1 3 1 4 1 3 1 2 3 4 3 2 3 1 0 0 0 0 1 4 1 1 0 1 1 1 0 0 1 "International Studies" 14 1 101 1 0 1 1 1 0 0 1 1 3 1 1 3 3 3 0 1 0 0 0 1 0 4 4 3 2 2 2 2 3 1 1 1 3 101 101 6 3 4 3 101 5 3 3 2 3 5 3 2 2 1 5 5 5 5 3 3 5 3 5 5 4 5 1 18 2 5 4 3 4 3 3 3 3 3 3)

  ,(Ans 3 5 4 1 2 4 4 3 3 4 3 3 3 2 3 1 2 2 2 2 3 2 1 3 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 13 1 101 1 0 1 1 1 0 0 1 1 3 101 101 101 101 3 1 1 0 0 1 0 0 4 4 4 3 3 3 3 4 3 3 2 2 101 101 6 5 5 5 3 5 3 5 3 5 2 2 3 4 4 4 4 3 4 3 3 3 3 4 4 4 4 1 18 1 5 4 3 3 4 2 3 3 3 3)

  ,(Ans 5 4 2 2 2 4 4 4 1 1 3 1 3 3 1 1 2 1 4 1 1 1 1 2 1 1 0 0 0 0 4 1 1 0 0 0 0 0 1 0 "Computer Science" 21 1 101 0 0 1 0 0 0 1 1 2 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 3 2 3 3 3 2 2 1 2 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 2 2 4 3 4 4 3 4 4 4 3 3 99 18 1 2 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 2 3 101 4 4 4 4 4 4 4 4 4 4 4 3 2 2 3 2 3 1 1 3 1 1 0 0 0 0 4 1 1 0 1 1 1 0 1 0 "Biology" 15 1 101 1 0 1 1 0 0 0 1 1 101 3 101 101 101 1 1 1 0 0 0 0 0 2 1 2 1 2 1 3 4 2 3 2 3 20 50 6 1 3 1 5 5 3 1 1 2 0 2 1 3 2 3 4 4 2 3 3 2 3 5 4 5 4 1 18 2 3 4 3 3 4 3 2 1 2 1)

  ,(Ans 6 3 4 2 4 4 4 4 3 3 3 3 2 2 1 3 2 2 3 3 3 3 2 3 2 1 0 0 0 0 3 1 0 2 0 0 1 0 0 0 "Applied Linguistics" 12 1 99 1 0 1 1 0 0 0 1 1 1 1 2 2 3 1 1 0 0 0 0 0 0 3 4 3 3 3 3 2 3 2 3 3 2 5 40 6 5 5 5 5 5 4 4 4 5 5 5 3 4 4 3 3 3 2 3 4 2 3 4 4 4 4 1 18 2 5 5 3 4 4 3 3 1 0 1)

  ,(Ans 6 6 4 1 4 2 3 1 4 4 4 2 4 3 3 4 2 2 2 3 3 2 1 4 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Mechanical Engineering" 13 1 101 1 0 1 0 1 0 0 1 2 3 2 2 1 2 3 1 0 0 0 0 0 0 4 3 3 1 4 3 3 2 2 1 3 2 101 101 6 5 5 5 5 4 4 3 5 5 1 5 2 3 3 4 4 3 3 3 3 3 3 3 4 2 3 0 18 1 4 4 3 2 4 3 3 1 2 1)

  ,(Ans 6 5 4 2 3 3 4 3 1 4 3 3 3 4 2 1 3 1 3 3 3 1 1 1 2 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Civil Engineering" 28 1 99 1 0 0 0 0 0 0 1 3 3 1 2 1 2 3 0 0 0 0 0 0 1 3 3 2 2 2 2 3 2 1 2 3 2 101 101 6 2 2 3 1 2 2 1 2 3 1 2 3 4 3 101 3 5 2 3 5 2 4 2 2 2 2 1 18 1 4 4 3 4 4 3 2 1 0 1)

  ,(Ans 6 3 4 4 4 3 3 4 2 3 3 2 4 3 3 3 4 1 4 2 2 1 3 2 1 0 0 0 1 0 4 1 1 1 0 0 1 0 0 0 "Computer Science" 12 1 101 1 0 1 1 1 0 0 1 1 2 1 2 2 3 1 1 0 0 0 0 0 0 4 2 3 3 4 4 4 1 3 2 4 3 5 15 6 1 3 3 1 3 2 1 2 2 1 1 2 2 3 3 3 4 1 3 4 2 3 3 3 2 1 0 18 1 4 4 3 3 2 3 3 1 2 1)

  ,(Ans 3 5 3 1 1 4 4 4 1 4 4 1 2 4 4 4 1 1 1 1 1 1 1 4 1 1 0 0 0 0 5 0 1 0 0 1 0 0 0 0 "Health Studies" 19 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 3 1 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 2 4 1 10 20 6 5 5 4 2 3 2 5 5 5 5 5 5 5 4 5 5 3 3 3 4 2 5 5 5 4 4 0 18 2 5 5 4 101 101 4 4 1 101 1)

  ,(Ans 6 5 4 1 3 4 3 4 2 3 3 3 3 4 4 4 3 2 3 3 3 1 1 4 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Mechanical Engineering" 14 1 101 0 0 0 0 0 0 1 1 6 1 1 2 3 3 1 1 1 0 0 0 0 0 4 4 4 3 4 4 4 3 3 2 4 4 3 20 6 4 5 5 3 5 4 5 3 5 2 5 2 2 3 5 5 3 3 2 3 3 3 3 4 3 4 0 18 1 4 2 3 4 4 3 3 1 2 1)

  ,(Ans 6 5 4 1 4 3 3 4 2 3 3 3 4 3 2 3 3 1 2 1 3 2 1 1 1 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Biology" 101 1 101 1 0 1 0 1 0 0 1 1 2 1 2 1 3 1 1 0 0 0 0 0 0 4 3 3 2 3 3 3 2 2 101 3 4 101 45 6 5 5 5 5 5 5 5 5 5 5 5 3 3 4 4 5 4 5 4 3 3 4 3 4 2 4 1 18 1 4 4 3 4 4 3 3 3 3 3)

  ,(Ans 6 2 4 1 1 4 4 4 4 4 3 4 1 1 1 1 3 4 3 3 1 1 3 3 3 0 0 0 1 0 3 0 0 0 0 0 1 0 0 0 "Health Studies" 14 1 99 1 0 1 1 1 0 0 1 3 3 3 1 2 2 3 1 0 0 0 0 0 0 101 101 101 101 101 101 4 4 4 4 4 2 101 101 6 5 5 5 5 5 5 5 3 5 3 4 3 4 3 4 4 4 4 2 4 3 4 2 4 3 3 1 18 2 5 4 3 3 4 3 3 3 0 3)

  ,(Ans 3 5 3 1 4 4 4 4 4 4 4 3 3 3 2 1 2 1 2 1 2 2 1 2 2 1 0 0 0 0 2 1 1 0 1 1 1 0 0 0 "Art" 12 1 101 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 3 2 2 2 3 3 2 1 1 1 3 3 101 101 6 5 5 5 3 4 4 3 4 5 3 5 3 3 3 3 3 2 2 2 3 2 3 3 4 2 3 0 18 2 3 2 3 3 3 3 3 3 0 3)

  ,(Ans 6 5 3 1 3 4 4 4 2 3 4 4 4 4 4 1 2 2 4 4 4 1 1 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "International Studies" 13 1 101 1 0 1 0 0 0 0 1 3 3 101 101 2 2 3 1 1 0 0 0 0 1 3 4 3 3 3 3 3 4 2 1 3 3 101 101 6 5 5 4 5 5 5 5 5 5 5 4 3 5 2 5 5 5 5 3 5 5 5 5 5 5 5 1 18 2 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 6 4 1 4 4 4 4 2 4 4 4 2 3 4 1 4 4 3 1 3 1 1 1 4 1 0 0 0 0 4 0 0 0 0 0 1 0 0 0 "Undecided" 13 1 101 1 0 0 0 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 3 3 3 3 3 3 3 3 1 1 3 2 101 101 6 5 5 5 5 5 3 5 3 3 2 1 1 2 3 4 3 3 1 2 2 3 2 4 4 3 4 1 18 2 2 2 3 4 4 3 3 0 0 0)

  ,(Ans 6 2 3 4 4 3 4 4 3 4 2 2 3 3 4 2 2 1 4 2 3 1 1 2 1 1 0 0 0 0 3 0 1 0 1 1 0 0 0 0 "Chemistry" 16 1 99 1 0 0 0 0 0 0 1 5 3 1 1 1 1 3 1 1 0 0 0 0 0 3 4 2 3 3 2 4 4 2 3 3 3 101 101 6 5 4 5 3 5 1 5 3 3 2 1 4 3 3 2 3 2 3 3 4 4 5 4 3 2 2 1 18 2 3 3 101 101 101 101 101 101 101 101)

  ,(Ans 3 5 3 1 3 4 4 4 3 4 3 101 4 1 4 4 101 1 1 1 1 3 1 1 1 0 1 0 0 0 2 1 1 0 1 0 1 0 1 0 "Undecided" 12 1 99 1 0 1 0 0 0 0 1 6 1 1 1 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 1 1 2 1 2 20 50 6 4 4 4 3 3 2 4 3 3 2 5 3 3 4 3 4 4 4 4 3 3 3 3 3 3 3 0 18 1 2 3 3 4 4 3 3 1 0 0)

  ,(Ans 3 6 3 101 3 3 4 101 4 2 4 4 3 101 101 4 101 101 101 101 4 101 3 4 101 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Elementary Education" 101 1 2 1 0 1 0 0 0 0 3 6 1 3 1 1 2 1 1 0 0 0 0 0 0 4 3 3 3 3 4 4 3 2 3 2 4 101 101 6 5 5 5 4 5 5 5 4 5 4 5 5 4 4 3 3 4 4 3 4 3 4 5 3 4 3 0 18 1 5 2 3 3 3 3 3 1 1 0)

  ,(Ans 6 3 4 1 3 4 3 4 2 4 3 2 3 4 2 1 1 1 3 2 2 1 4 3 1 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Administration of Justice" 15 1 101 1 0 1 1 1 0 0 1 1 3 1 1 1 1 3 0 0 0 0 1 0 0 4 3 3 3 3 3 4 2 2 1 2 1 101 101 6 5 5 5 5 5 5 5 5 5 5 0 101 101 101 101 101 101 101 101 101 101 101 4 5 5 5 1 18 2 3 99 2 3 3 3 3 1 2 1)

  ,(Ans 3 6 3 1 2 4 4 4 4 3 4 2 2 4 4 1 2 1 2 2 2 1 1 3 1 1 0 0 0 0 2 1 1 0 0 0 0 0 0 0 "Architecture" 15 1 101 1 0 1 1 1 0 0 1 1 3 1 2 1 2 3 0 0 0 0 0 0 0 3 3 3 2 3 2 3 3 3 2 3 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 2 2 3 2 2 2 1 2 2 3 4 4 4 1 18 2 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 2 3 1 4 4 3 4 1 4 3 4 3 3 3 1 3 2 3 2 4 2 2 2 4 1 0 0 0 0 3 1 1 1 0 0 0 1 0 0 "Administration of Justice" 16 1 101 1 0 0 0 0 0 0 1 6 3 101 101 101 101 3 1 0 1 0 0 1 0 3 4 4 2 2 3 101 101 101 101 101 101 101 101 6 1 4 2 5 5 3 5 0 1 2 2 1 2 1 5 5 3 4 1 3 4 1 4 4 4 5 1 18 2 2 2 3 3 4 3 3 3 3 3)

  ,(Ans 6 2 4 3 3 3 4 4 101 3 2 4 3 4 2 4 2 2 3 2 2 1 3 2 4 1 0 0 0 0 3 1 1 1 0 0 0 0 0 1 "Science" 15 1 99 1 0 1 0 0 0 0 1 6 2 3 1 1 3 1 1 0 0 0 0 0 0 4 4 3 4 3 3 3 3 3 4 3 3 8 45 6 2 3 4 1 4 5 5 101 2 4 5 2 2 2 2 2 2 2 2 2 2 2 4 4 5 5 1 18 1 2 2 3 2 4 2 2 3 0 3)

  ,(Ans 6 1 4 2 4 4 4 4 3 3 2 4 1 4 2 2 3 1 3 2 2 2 1 2 4 1 0 0 0 1 3 1 1 7 1 0 0 0 0 0 "Biology" 14 1 101 1 0 1 0 0 0 0 1 2 1 2 1 1 3 1 0 0 0 0 0 0 0 3 3 3 3 3 3 3 2 2 2 2 2 12 72 6 5 5 5 3 4 5 5 4 5 3 5 4 3 4 3 3 4 3 3 3 3 4 4 3 3 3 1 18 2 3 3 3 4 4 3 3 0 0 0)

  ,(Ans 3 3 3 4 4 4 4 2 4 4 2 1 1 3 1 4 4 4 4 4 4 101 2 2 4 0 0 0 1 0 4 1 1 0 0 0 0 0 0 1 "Did not answer" 13 1 101 1 0 0 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 1 0 4 4 4 2 2 2 4 4 4 2 2 2 101 101 6 5 5 1 5 2 1 5 3 3 1 4 1 1 2 4 3 1 2 2 2 1 3 3 3 2 2 1 18 2 2 2 3 2 2 2 2 3 0 3)

  ,(Ans 6 6 3 3 3 4 4 4 1 4 3 3 3 4 3 1 3 4 3 3 4 2 3 3 4 0 0 0 1 0 101 1 1 7 0 0 0 0 0 0 "Health Studies" 101 1 1 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 0 0 0 0 0 0 1 4 4 4 4 4 3 3 2 2 4 4 3 101 101 6 4 2 4 4 4 4 1 2 3 4 3 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 1 18 2 4 4 3 3 3 3 3 0 0 0)

  ,(Ans 6 3 4 1 4 4 4 4 3 3 3 2 3 4 2 1 1 1 3 3 3 2 1 2 2 1 0 0 0 0 4 1 1 0 0 0 0 0 0 0 "Mechanical Engineering" 16 1 101 1 0 1 1 1 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 1 4 3 3 3 3 3 3 3 3 3 2 3 101 101 6 4 4 4 4 5 5 5 5 5 3 5 2 2 2 4 4 4 2 3 3 4 2 2 4 3 3 1 18 2 5 2 3 3 3 3 3 3 2 3)

  ,(Ans 6 3 4 4 4 4 4 4 3 3 4 3 4 3 3 1 3 3 3 3 3 2 1 3 2 1 0 0 0 0 3 1 1 0 1 1 1 0 0 0 "Psychology" 15 1 101 1 0 1 1 1 0 0 1 1 3 1 1 2 2 3 1 0 0 0 1 0 1 4 4 3 3 3 3 4 4 3 3 3 3 101 101 6 5 5 5 5 5 4 3 4 5 4 4 3 4 4 5 4 2 3 3 4 3 4 4 4 4 4 1 18 2 2 2 3 3 2 3 3 3 3 3)

  ,(Ans 3 3 3 1 1 4 4 2 2 2 3 1 4 3 3 1 1 1 1 1 1 1 1 4 1 1 0 0 0 0 5 1 0 99 0 0 0 0 0 1 "International Studies" 14 1 101 1 0 1 1 1 0 0 1 2 3 1 1 2 2 3 0 1 0 0 0 1 1 4 4 4 4 4 4 2 2 2 2 4 4 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 4 3 5 5 5 4 5 5 5 5 3 4 3 4 1 18 2 3 3 3 4 4 3 3 3 3 3)

  ,(Ans 3 4 3 1 3 4 4 4 3 2 4 4 3 2 4 1 3 1 3 3 4 3 4 4 1 1 0 0 0 0 4 0 1 7 1 0 0 1 0 1 "History" 13 1 101 1 0 1 1 1 0 0 1 1 3 1 1 2 2 3 1 0 0 0 0 0 0 4 4 2 2 4 4 3 3 3 2 4 4 101 101 6 5 5 2 3 2 3 5 5 5 5 5 4 4 3 2 2 2 3 5 5 1 5 5 3 4 4 0 18 1 2 4 3 4 4 3 3 3 3 3)

  ,(Ans 3 5 3 2 2 4 3 2 3 2 4 2 4 2 2 1 2 3 2 1 3 1 1 4 2 1 1 0 0 0 3 0 1 7 0 0 0 0 0 1 "Philosophy" 15 1 99 1 0 1 1 0 0 0 1 2 3 101 101 101 101 3 1 1 0 0 0 0 0 101 101 101 101 101 101 4 4 4 3 4 4 101 101 6 1 2 2 2 5 3 3 2 3 1 5 2 2 3 4 4 2 4 3 3 2 4 4 5 3 3 1 18 1 5 5 3 3 3 3 3 3 2 3)

  ,(Ans 5 5 3 1 1 4 4 1 2 2 3 4 1 1 4 4 3 1 3 1 2 2 2 3 3 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Did not answer" 13 1 101 1 0 0 1 1 0 0 1 1 1 1 1 1 3 1 0 0 0 0 0 0 0 101 101 101 101 101 101 3 1 1 1 3 1 101 15 6 5 5 5 4 3 4 3 101 5 5 3 5 5 3 3 4 4 2 101 5 4 5 5 5 3 3 0 18 2 4 2 3 101 4 3 3 3 101 3)

  ,(Ans 3 5 3 3 3 4 4 2 2 2 4 4 3 3 4 2 4 1 3 3 3 3 3 3 3 1 0 0 0 0 3 0 1 0 0 0 0 0 0 0 "Undecided" 101 1 101 1 0 0 0 0 0 0 1 1 3 101 101 101 101 3 1 0 0 0 0 0 0 4 4 4 4 4 4 101 101 101 101 101 101 101 101 6 5 5 5 5 5 3 2 3 3 2 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 18 1 5 3 3 2 2 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 3 3 2 3 2 1 3 4 1 1 3 1 3 2 3 1 2 2 1 1 0 0 0 0 2 1 1 0 0 0 0 0 0 1 "Undecided" 14 1 101 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 0 0 0 1 0 0 4 2 2 3 3 4 4 1 2 1 3 1 101 101 6 5 5 5 3 5 4 5 3 5 0 5 3 3 5 5 5 4 5 3 3 2 4 3 4 99 99 0 18 1 2 3 3 3 4 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 4 4 4 4 4 1 4 1 4 1 4 4 4 4 4 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Psychology" 15 1 101 1 0 0 0 0 0 0 1 6 3 1 1 1 3 3 1 0 1 0 0 1 1 4 3 3 3 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 2 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 1 18 2 2 2 3 4 4 3 3 3 2 3)

  ,(Ans 6 3 3 1 4 4 4 4 3 3 4 3 4 3 3 1 3 1 1 3 3 3 3 4 1 1 0 0 0 0 3 0 1 0 1 1 1 0 0 0 "Elementary Education" 12 1 101 1 0 1 0 0 0 0 1 1 3 1 1 2 1 3 1 1 0 0 0 0 1 3 3 3 101 3 101 101 101 101 4 101 2 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 3 4 1 2 4 4 4 4 4 4 1 18 2 2 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 3 3 4 4 4 4 4 4 4 3 4 4 4 3 2 3 3 3 3 4 3 1 4 4 1 0 1 0 0 4 1 1 0 1 1 0 0 0 0 "Accounting" 14 1 101 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 1 0 0 0 1 1 4 4 4 4 101 101 101 101 101 4 4 3 101 101 6 4 5 2 5 5 3 5 2 2 3 1 1 2 1 3 3 3 3 3 3 3 3 4 5 4 5 1 18 2 3 2 3 4 4 2 3 2 2 2)

  ,(Ans 6 3 4 1 1 2 4 4 1 2 4 4 4 4 4 1 2 1 2 1 2 3 1 3 1 1 0 0 0 0 1 1 0 0 1 0 0 0 0 0 "Business Administration" 15 1 99 1 1 1 1 0 0 0 1 1 3 1 2 1 3 3 1 0 0 0 1 0 1 4 3 3 3 3 1 3 1 101 1 1 1 101 101 6 5 5 1 3 5 5 5 5 5 2 5 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 1 18 1 2 3 3 4 4 3 3 3 0 3)

  ,(Ans 6 2 4 4 4 3 4 4 2 3 1 4 2 3 3 4 2 1 1 1 2 1 1 1 3 1 0 0 0 0 4 1 1 7 1 1 0 1 0 0 "Health Studies" 14 1 101 1 0 0 0 0 0 1 1 2 1 1 1 1 3 1 1 1 0 0 0 0 0 3 3 3 2 3 3 2 1 1 1 3 3 10 90 6 5 5 4 5 5 3 5 4 5 5 2 3 3 2 4 4 2 5 3 3 5 3 4 3 1 2 1 18 2 2 1 3 4 4 1 2 3 2 3)

  ,(Ans 6 5 4 3 4 4 4 3 2 4 4 3 4 3 3 4 3 2 4 3 4 3 2 4 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "English" 13 1 101 1 0 1 1 1 0 0 1 6 3 1 2 1 2 3 0 1 0 0 0 0 1 4 4 4 4 4 4 4 3 3 3 4 4 101 101 6 4 5 3 5 4 3 5 5 5 5 5 3 4 2 4 5 2 2 3 3 3 2 5 5 4 4 1 18 2 3 4 3 4 3 3 3 3 2 3)

  ,(Ans 5 6 3 1 4 2 3 4 4 4 1 4 1 4 4 4 1 1 1 1 1 1 1 1 2 1 0 0 0 0 3 1 0 0 0 0 1 0 0 0 "Administration of Justice" 13 1 101 1 0 1 1 1 0 0 1 1 1 1 1 1 3 1 1 0 0 0 0 0 0 4 3 4 2 4 2 4 3 3 2 4 2 20 30 6 5 5 5 5 5 2 5 2 5 1 5 3 3 3 3 3 3 3 3 3 2 3 4 4 4 4 1 18 1 3 5 3 2 4 3 3 3 0 3)

  ,(Ans 6 4 4 3 4 4 4 4 3 3 4 3 4 4 4 4 3 3 4 4 3 2 2 4 3 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Undecided" 14 1 101 1 0 0 0 0 0 0 1 2 101 101 101 101 3 1 1 0 0 0 0 0 0 4 101 101 3 3 4 101 3 3 101 101 101 45 45 6 5 5 4 5 4 5 5 5 4 4 3 4 5 4 4 4 5 3 4 4 4 5 5 5 5 5 1 18 2 4 4 3 3 4 3 3 0 0 0)

  ,(Ans 6 6 4 3 4 4 3 1 3 4 4 4 4 4 2 1 3 3 4 3 2 2 1 4 3 1 1 0 0 0 3 1 1 0 1 0 0 0 0 0 "Business Administration" 15 1 1 1 0 0 0 0 0 0 1 1 3 1 1 1 3 3 0 0 0 0 1 0 1 4 4 4 3 3 3 4 4 4 3 3 3 101 101 6 4 4 3 2 2 3 2 2 3 3 3 3 3 2 4 4 4 5 2 2 2 2 3 3 4 3 1 18 1 4 4 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 1 4 4 4 3 2 3 2 1 2 4 3 1 3 1 2 3 2 1 2 2 1 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Business Administration" 13 1 99 1 0 0 0 0 0 0 1 6 3 101 101 101 101 3 1 1 0 0 0 0 0 3 2 3 3 3 1 3 2 2 2 3 1 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 3 3 3 3 4 4 2 3 4 4 4 4 3 3 1 18 2 2 3 3 2 2 3 3 1 1 1)

  ,(Ans 6 4 3 1 1 3 4 4 2 1 1 1 4 3 1 4 2 1 3 2 2 2 1 3 2 1 0 0 0 0 4 1 0 0 0 0 1 0 0 1 "Biology" 17 1 101 1 0 1 0 0 0 0 1 6 101 3 2 101 101 1 1 1 0 0 0 0 0 4 3 4 1 1 3 4 3 4 1 1 3 7 15 6 5 5 5 4 5 5 5 4 5 4 5 3 2 3 5 5 5 4 3 4 2 3 5 4 3 3 1 18 2 4 5 3 4 4 3 3 0 2 1)

  ,(Ans 3 6 3 1 4 4 4 1 4 4 3 2 3 3 1 1 1 1 1 1 1 3 1 1 1 1 0 0 0 0 3 1 1 7 1 1 0 1 0 0 "Business Administration" 16 1 101 1 0 1 1 1 0 0 1 6 3 1 1 1 1 3 0 0 0 0 0 0 0 3 4 3 1 4 4 2 1 1 1 4 1 101 101 5 5 5 5 5 5 5 5 5 5 3 5 4 4 4 2 3 3 3 3 4 1 4 5 5 3 4 1 18 2 3 2 3 4 4 3 3 1 1 1)

  ,(Ans 6 3 4 1 4 4 4 4 4 4 3 4 2 3 3 4 3 3 4 3 3 2 1 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 12 1 101 1 0 0 0 0 0 0 3 3 2 1 1 1 3 1 0 1 0 0 0 1 0 4 4 3 2 2 3 3 2 3 2 2 4 30 68 6 5 5 5 5 5 5 5 5 5 5 5 3 3 4 5 5 4 3 3 3 3 3 4 5 4 5 1 18 2 3 2 3 3 4 2 3 0 0 1)

  ,(Ans 6 6 4 4 4 4 4 4 4 3 3 2 3 4 3 1 3 1 4 3 4 3 101 2 1 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Undecided" 12 1 101 1 0 1 0 1 0 0 1 3 1 2 3 1 1 1 0 1 0 0 0 0 0 3 2 1 3 1 2 3 1 1 1 1 2 8 15 6 1 5 5 0 0 0 0 0 0 0 0 2 3 3 2 1 1 1 1 1 101 2 3 3 3 3 0 18 2 3 4 3 3 4 3 3 1 0 1)

  ,(Ans 6 6 101 4 3 4 4 3 4 1 3 1 1 4 3 4 1 1 1 3 4 4 1 3 4 1 0 0 0 0 2 0 1 0 0 1 1 1 0 0 "Art" 13 1 101 1 0 1 1 0 0 0 1 1 1 1 1 2 3 1 1 0 1 0 1 1 0 4 3 2 1 4 2 4 2 2 1 4 2 20 45 6 5 5 4 3 5 3 3 5 1 0 5 3 2 5 2 2 5 1 5 4 1 4 4 5 5 5 1 18 2 5 3 3 4 2 3 2 0 0 0)

  ,(Ans 6 2 4 3 4 4 4 4 3 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 0 0 0 0 3 1 1 7 0 1 0 0 0 0 "Chemistry" 14 2 2 1 0 0 0 0 0 0 1 2 1 2 2 1 2 101 0 0 0 0 0 0 0 101 101 101 101 101 101 101 101 101 101 101 101 9 35 6 5 5 1 1 1 1 2 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 4 4 4 4 0 18 1 2 5 3 101 101 3 101 0 0 0)

  ,(Ans 5 5 2 4 4 3 4 4 3 4 3 3 1 3 1 2 2 1 3 1 1 2 3 2 3 1 0 0 0 0 2 1 0 7 1 1 1 1 0 0 "Undecided" 15 5 2 1 1 1 1 1 0 0 1 2 1 2 3 1 1 1 0 0 0 0 0 0 0 3 3 2 1 3 2 2 2 1 1 3 1 15 20 6 4 5 5 2 5 5 3 1 0 0 5 1 2 2 3 3 3 1 1 1 1 3 3 3 2 3 0 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 4 4 3 4 3 4 4 2 4 2 2 1 2 1 2 1 2 2 1 2 2 1 0 0 0 0 3 0 1 1 0 0 0 0 0 0 "English" 13 5 2 1 0 1 0 1 0 0 1 6 2 2 101 101 2 1 1 0 0 0 1 0 0 1 1 2 1 1 1 4 3 3 3 3 4 20 15 6 0 5 5 5 5 5 5 5 5 5 5 1 4 4 4 4 4 3 3 4 3 3 5 5 3 3 1 18 2 4 2 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 2 4 4 4 2 3 4 4 3 2 3 3 2 3 2 2 2 3 2 3 3 2 0 1 0 0 0 2 1 1 7 1 0 0 0 0 0 "Undecided" 13 5 2 1 0 1 1 1 0 0 101 2 1 3 3 101 101 3 0 0 0 0 1 0 0 3 2 3 2 3 3 3 2 3 2 4 3 101 101 6 5 5 5 4 4 5 5 5 5 2 3 4 3 3 4 4 3 3 2 4 2 2 4 4 2 3 0 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 3 3 4 4 1 4 2 3 4 4 3 4 1 2 1 1 1 1 1 4 4 1 1 0 0 0 0 4 1 1 1 1 1 0 1 0 0 "Business Administration" 13 4 101 1 0 1 1 1 0 0 1 1 3 1 1 1 3 1 1 1 0 0 0 0 0 4 4 3 4 2 2 3 3 3 3 3 1 10 90 6 5 5 3 4 2 5 5 2 5 0 3 4 4 2 2 3 5 4 1 3 1 3 4 3 5 3 1 18 2 5 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 2 4 1 2 4 3 4 3 4 3 1 3 4 1 1 3 2 2 4 2 2 1 3 4 1 0 0 0 1 4 1 1 7 0 0 1 0 0 0 "Business Administration" 13 4 2 1 0 1 0 0 0 0 1 6 3 101 101 101 101 3 1 1 0 1 0 0 1 4 4 3 2 4 4 4 3 2 2 4 3 101 101 6 0 5 5 5 5 4 5 1 5 5 5 101 5 4 5 5 5 5 2 5 5 5 4 5 3 4 1 18 2 99 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 4 4 3 3 1 3 3 2 4 3 3 4 1 2 1 3 1 3 1 1 2 4 0 1 0 0 0 3 1 1 4 0 0 0 0 0 0 "Administration of Justice" 17 3 2 1 0 1 1 1 0 0 1 3 3 1 1 2 2 3 1 0 0 0 0 0 0 3 2 3 2 4 3 4 2 3 2 4 3 101 101 6 5 4 5 5 5 4 5 5 5 5 5 2 2 4 4 4 4 2 4 3 4 4 5 4 5 5 1 18 1 5 5 3 2 2 3 3 3 3 3)

  ,(Ans 6 5 3 1 2 4 3 3 2 2 2 4 3 3 1 1 1 1 3 4 3 2 1 2 2 1 0 0 0 0 3 1 1 7 0 0 0 0 1 1 "Undecided" 13 3 2 1 0 1 1 0 0 0 1 2 3 1 1 1 2 3 0 1 0 0 0 0 0 4 4 3 3 3 4 4 3 3 3 2 4 101 101 6 5 5 4 5 5 4 5 5 5 3 5 3 2 4 5 5 3 5 4 4 2 4 4 2 2 3 1 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 5 5 101 3 3 3 3 3 3 3 3 4 4 2 2 2 3 2 3 3 3 3 3 3 3 1 0 0 0 0 4 0 1 0 1 1 0 0 0 0 "Undecided" 15 3 2 1 0 0 0 0 0 0 1 6 2 101 101 101 3 1 0 0 0 0 0 0 0 3 4 3 3 3 3 101 101 101 101 101 101 4 50 6 2 5 5 2 5 5 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 4 4 3 3 1 18 1 2 1 101 101 101 101 101 101 101 101)

  ,(Ans 3 3 3 1 4 4 4 2 3 4 4 3 4 4 4 1 3 2 4 2 3 3 1 4 3 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Did not answer" 15 3 2 1 0 0 0 0 0 0 1 1 3 1 1 1 2 3 1 0 0 0 0 0 0 4 4 4 2 1 4 4 3 3 2 1 3 101 101 6 3 5 2 5 5 2 5 0 3 3 5 3 4 3 4 4 4 4 3 5 5 5 5 5 4 5 1 18 2 5 3 3 3 2 3 3 1 1 1)

  ,(Ans 3 4 2 1 3 4 2 2 3 4 2 4 3 3 3 1 2 1 1 1 1 2 1 1 3 1 0 0 0 0 3 1 1 7 0 0 0 0 1 0 "Music" 16 3 101 1 0 0 0 0 0 0 1 2 3 2 1 1 1 3 0 0 0 0 0 0 0 2 3 3 3 101 101 1 101 101 101 101 101 101 101 5 0 3 3 0 0 0 1 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 0 18 1 2 2 2 1 1 1 1 101 101 101)

  ,(Ans 6 2 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 0 0 0 5 1 1 2 0 0 0 0 0 0 "Political Science" 13 3 2 1 0 1 0 0 0 1 101 3 101 3 101 101 101 1 1 1 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 5 30 6 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 101 18 2 101 101 101 101 101 101 101 1 101 101)

  ,(Ans 6 5 4 2 2 4 4 3 2 4 4 3 4 4 2 3 101 2 4 3 4 4 1 4 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Business Administration" 14 2 2 1 0 1 1 0 0 1 1 2 1 2 2 1 3 1 0 0 0 0 1 0 0 4 4 4 3 4 4 4 3 2 2 4 3 10 15 6 5 5 5 3 5 5 5 0 5 5 5 3 3 2 2 4 5 4 2 4 3 4 4 5 3 4 1 18 1 2 3 101 101 101 101 101 101 101 101)

  ,(Ans 5 2 3 1 4 4 4 4 4 4 4 3 4 4 3 4 4 1 4 4 4 2 1 4 2 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Political Science" 15 2 2 1 0 1 1 1 0 0 1 1 2 3 2 1 2 1 1 1 1 0 0 0 0 1 1 1 3 2 3 4 3 3 2 2 3 5 20 6 5 3 3 2 5 1 5 2 5 4 3 3 4 2 5 5 2 4 3 3 4 3 4 3 4 4 1 18 2 5 5 3 4 4 3 2 3 2 3)

  ,(Ans 6 6 4 4 4 4 4 1 4 4 4 4 4 4 4 3 3 1 2 2 4 3 2 4 4 1 0 0 0 0 3 0 0 7 0 0 0 0 0 1 "Biology" 15 2 1 1 0 1 1 1 0 0 1 3 3 2 2 2 2 3 1 0 0 0 0 0 0 3 3 3 3 3 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 1 18 2 2 4 3 4 4 3 3 3 0 0)

  ,(Ans 6 5 4 1 2 3 4 1 3 3 3 4 4 3 3 4 3 1 3 1 1 1 3 1 1 1 0 0 0 0 4 1 1 7 0 0 1 0 0 0 "Theater Arts" 13 2 2 1 0 1 1 1 0 0 1 1 3 1 1 1 3 1 0 0 0 0 0 0 0 4 3 3 2 2 2 2 1 1 1 1 1 20 90 6 4 4 4 2 5 5 5 5 5 3 3 2 4 4 3 3 4 4 3 3 1 4 4 3 2 4 1 18 1 2 4 101 101 101 101 101 101 101 101)

  ,(Ans 99 3 4 3 4 4 3 3 4 4 4 4 3 4 2 3 3 1 3 4 3 3 2 3 4 0 0 0 0 1 4 1 1 0 0 0 1 0 0 0 "Mathematics" 14 2 2 1 0 1 1 1 0 0 1 1 2 1 1 2 3 2 0 0 0 0 0 0 0 1 2 3 2 2 1 3 2 1 2 2 4 2 20 6 1 3 3 5 5 3 3 1 2 2 3 1 2 2 4 4 2 1 1 2 2 1 4 3 3 4 1 18 1 2 2 3 3 2 3 3 1 0 1)

  ,(Ans 6 6 3 1 4 4 4 2 4 4 2 3 3 2 4 4 2 2 3 2 2 4 1 2 3 1 0 0 0 0 4 101 1 0 0 0 1 0 0 0 "Elementary Education" 15 2 2 1 0 0 0 0 0 0 1 3 1 1 1 1 3 1 0 0 0 0 0 0 0 4 3 3 2 4 4 2 2 2 1 3 4 10 45 6 5 5 5 3 5 5 5 4 5 2 5 3 3 2 3 3 3 3 3 3 3 3 3 4 4 5 1 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 2 4 1 4 4 3 4 2 4 3 4 4 3 3 3 2 1 101 2 2 2 1 1 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Philosophy" 15 2 2 1 0 1 1 1 0 0 1 2 2 101 101 101 3 1 1 0 0 0 0 0 0 3 3 2 2 3 3 4 3 2 1 2 2 20 65 6 3 3 3 5 5 5 5 3 4 5 5 4 4 3 5 5 2 5 3 4 3 4 3 3 4 4 1 18 1 3 2 3 4 4 3 3 3 0 3)

  ,(Ans 6 6 4 4 4 4 4 3 4 4 4 4 4 4 2 4 4 3 4 4 4 4 1 4 1 1 0 0 0 0 5 1 1 0 0 0 1 1 0 0 "Business Administration" 16 2 2 1 0 0 1 0 0 0 1 1 1 1 1 1 3 1 1 0 0 0 1 0 0 4 3 4 4 4 2 3 3 4 4 4 4 20 60 6 5 4 3 2 2 4 5 5 4 1 5 4 3 1 4 4 5 2 2 2 2 1 5 5 5 5 1 18 1 3 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 1 4 4 4 4 2 2 4 1 2 4 2 1 3 1 4 3 2 1 1 4 2 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Foreign Languages" 15 1 101 1 0 1 1 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 1 0 1 4 4 3 3 4 3 4 3 3 101 4 2 101 101 6 5 5 4 3 5 5 5 5 5 4 2 5 4 5 5 5 5 3 5 5 5 3 4 4 5 5 1 18 1 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 3 4 3 3 2 2 1 1 3 4 101 3 1 1 3 1 1 1 1 2 1 1 2 1 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Undecided" 4 1 101 1 0 1 0 0 0 0 1 1 3 1 1 1 1 3 0 0 0 0 0 0 0 3 2 2 2 2 3 1 1 2 1 1 2 101 101 6 5 4 5 3 5 5 5 5 5 3 5 4 2 4 3 3 3 2 3 4 3 5 3 4 3 3 0 18 1 3 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 2 3 1 4 3 3 3 1 4 2 1 4 3 3 1 2 1 2 1 1 1 1 1 1 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Did not answer" 14 1 101 1 0 1 0 1 0 0 1 1 3 1 1 1 2 3 0 0 0 0 0 0 0 4 3 4 1 2 4 3 2 3 1 1 3 101 101 6 1 3 4 1 2 0 4 1 3 0 5 1 2 3 3 3 2 1 2 3 3 3 1 3 1 1 0 18 1 2 5 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 4 1 4 2 3 2 2 4 3 4 2 2 3 1 2 4 2 1 2 1 1 3 3 0 0 0 1 0 3 0 0 0 0 0 1 0 0 0 "Did not answer" 12 1 101 1 0 1 0 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 3 3 101 2 2 2 2 2 2 1 1 1 101 101 6 4 4 3 2 2 2 2 1 2 2 2 1 1 1 1 1 1 1 1 1 101 1 3 101 3 3 1 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 3 1 101 4 4 1 4 4 3 3 3 2 1 1 2 2 2 1 3 2 1 3 3 1 0 0 0 0 3 1 1 0 0 0 1 0 0 0 "Child and Family Studies" 17 1 101 1 0 1 0 0 0 0 1 3 3 101 101 101 101 3 0 0 0 0 0 0 0 4 3 2 2 3 4 3 2 2 1 2 4 101 101 6 5 4 4 5 5 3 5 1 4 3 5 4 3 2 2 1 3 3 3 4 3 4 4 3 3 4 1 18 2 2 99 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 3 1 4 4 4 2 2 4 4 4 4 4 4 2 2 3 3 3 3 2 1 2 1 1 0 0 0 0 3 1 1 0 0 0 0 1 0 0 "Engineering" 14 1 101 1 0 1 1 1 0 0 3 1 2 1 1 2 3 1 0 0 0 0 0 1 0 4 4 101 4 4 4 4 4 101 3 2 3 2 23 6 5 5 5 1 5 3 5 5 5 5 5 4 4 4 3 4 3 5 4 4 3 5 3 4 2 2 1 18 101 101 101 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 3 1 4 4 3 4 3 3 2 3 2 4 3 3 2 1 3 3 2 2 1 2 3 1 1 0 0 0 3 1 1 0 0 0 0 0 0 0 "Architecture" 101 1 101 1 0 0 0 0 0 0 1 6 101 3 101 101 101 2 0 0 0 0 1 1 0 3 3 101 101 101 101 3 3 101 101 101 101 10 20 6 5 5 3 5 5 5 3 0 5 4 5 101 101 101 101 101 101 101 101 101 101 101 5 5 5 5 1 18 1 3 4 3 3 101 3 3 3 101 3)

  ,(Ans 6 4 3 1 4 4 4 4 1 2 3 4 2 4 4 3 4 1 4 1 3 3 2 3 1 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Psychology" 15 5 2 1 0 0 0 0 0 0 1 1 1 3 2 1 1 1 1 0 0 0 1 0 0 4 3 4 1 3 4 3 3 3 1 3 4 25 20 6 5 5 4 5 4 4 3 5 5 5 4 5 5 3 4 4 3 4 5 5 4 4 5 4 5 5 1 17 2 5 5 3 3 3 3 3 2 2 3)

  ,(Ans 6 6 4 2 4 4 4 4 4 2 3 3 1 3 3 1 1 1 3 3 2 3 1 3 2 0 1 0 1 0 2 1 0 7 0 0 0 0 0 1 "International Studies" 12 5 2 1 0 0 0 0 0 0 1 1 3 101 101 101 3 2 1 1 0 0 0 0 0 1 1 1 1 1 1 101 101 101 101 101 101 5 20 6 5 5 5 5 5 5 5 5 5 5 5 2 3 2 4 4 4 3 3 4 4 4 3 4 3 3 1 17 2 2 2 3 3 3 3 3 2 2 2)

  ,(Ans 6 6 3 1 1 1 1 3 1 1 1 4 1 4 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 1 2 1 1 1 1 1 1 0 0 0 "Political Science" 12 4 2 0 0 1 0 0 1 0 1 1 1 1 1 1 3 2 1 1 1 0 0 0 0 4 4 4 1 4 4 4 4 4 1 4 4 10 15 6 1 0 2 5 5 2 2 1 1 2 1 1 1 1 3 3 3 3 2 2 2 2 3 4 3 3 1 17 1 99 99 1 4 4 1 1 0 2 1)

  ,(Ans 6 5 4 2 4 4 3 3 2 3 4 3 4 4 3 2 3 1 4 2 3 2 1 2 3 1 0 0 0 0 4 1 1 0 0 0 0 1 0 0 "Computer Science" 15 4 2 1 0 1 1 1 0 0 1 6 3 1 1 2 2 3 1 0 0 1 1 0 0 3 3 3 2 3 3 3 2 2 1 3 3 101 101 6 3 3 5 4 3 2 2 4 3 4 4 3 2 4 4 3 3 2 3 4 3 5 3 4 3 3 0 17 1 4 4 3 3 4 3 3 1 2 1)

  ,(Ans 6 3 4 1 3 4 4 3 2 3 3 4 3 3 2 1 2 1 4 2 4 3 3 4 4 0 0 1 0 0 4 1 1 0 0 0 0 0 0 0 "Art" 13 4 2 1 0 1 1 1 0 0 1 6 2 2 101 3 2 2 0 1 0 0 0 0 0 4 3 3 2 2 3 101 101 101 101 101 101 4 15 6 5 5 5 5 5 5 4 5 5 5 5 4 4 4 4 4 2 3 3 4 4 4 5 5 5 5 1 17 2 2 3 3 3 3 3 3 1 2 1)

  ,(Ans 6 6 4 1 4 4 4 1 2 4 4 4 4 4 2 4 3 2 2 2 3 1 2 3 4 1 0 0 0 0 2 1 1 7 0 0 0 0 1 0 "Did not answer" 15 4 2 1 0 1 1 1 0 0 1 3 1 2 2 2 3 1 1 1 1 0 1 1 1 2 3 3 3 3 3 101 101 101 101 101 101 40 50 3 5 5 5 5 5 5 4 3 4 5 2 3 3 3 3 3 3 3 3 3 3 3 5 5 5 5 1 17 1 3 2 3 2 2 3 3 3 0 3)

  ,(Ans 101 3 4 4 4 4 4 1 3 3 4 3 4 4 4 1 3 1 4 4 4 1 2 4 3 1 0 0 0 0 5 1 1 7 0 0 0 0 0 0 "Psychology" 14 4 2 1 0 1 1 1 0 0 1 2 3 1 1 3 1 3 1 1 0 0 0 0 1 4 4 4 3 4 4 3 3 3 3 4 4 101 101 6 5 5 5 5 5 5 5 0 101 5 5 2 5 5 5 5 3 4 1 5 5 5 5 5 4 5 1 17 2 3 4 3 4 4 3 3 2 2 2)

  ,(Ans 6 5 4 3 4 4 4 3 2 2 4 4 3 4 4 1 3 1 4 1 2 2 1 4 4 1 0 0 1 0 3 1 1 7 0 0 1 0 0 0 "Child and Family Studies" 16 3 101 1 0 1 0 1 0 0 1 3 3 1 1 2 3 3 1 0 0 0 0 0 1 3 3 3 3 3 3 3 3 3 3 3 3 101 101 6 3 5 5 4 3 5 4 2 2 2 3 2 3 3 3 3 3 3 3 3 3 3 4 4 3 4 1 17 2 3 4 3 3 4 3 3 2 2 2)

  ,(Ans 3 5 2 3 3 4 4 2 3 3 2 4 3 2 4 1 3 1 1 1 2 1 2 3 3 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Undecided" 13 3 1 1 0 1 0 0 0 0 1 2 3 1 1 1 2 3 1 0 0 0 0 0 0 3 3 3 2 4 3 3 3 2 1 4 2 101 101 6 4 4 4 1 5 1 5 1 2 0 3 3 3 2 3 2 2 1 2 2 3 2 4 4 3 3 0 17 2 2 4 3 4 3 3 3 2 2 2)

  ,(Ans 6 3 4 3 4 4 4 4 4 4 4 2 2 3 4 4 4 2 3 3 3 1 3 4 4 0 1 0 0 0 5 1 1 0 0 0 0 1 0 0 "Computer Engineering" 15 3 2 1 1 1 0 1 0 0 1 1 1 3 2 1 2 2 1 1 0 0 0 0 0 4 4 3 3 4 2 4 4 3 3 4 2 15 25 6 4 3 4 2 5 5 5 4 4 5 4 3 2 4 4 5 5 2 3 3 3 3 4 4 4 4 0 17 1 1 1 3 3 3 3 3 2 2 2)

  ,(Ans 3 3 2 1 4 1 1 1 1 4 1 3 3 2 1 1 1 1 1 1 1 1 1 3 2 1 0 0 0 0 2 1 1 7 0 0 1 0 0 0 "Electrical Engineering" 12 3 101 1 0 1 0 1 0 0 1 6 3 1 1 1 2 3 0 0 0 0 0 0 0 2 2 3 2 3 2 1 1 1 1 2 1 101 101 6 5 5 5 5 5 5 5 5 5 0 5 3 3 3 3 3 5 2 4 3 3 3 1 2 5 4 1 17 1 3 5 3 3 4 3 3 0 2 0)

  ,(Ans 6 3 4 3 4 4 4 4 2 4 4 4 2 4 3 101 4 4 4 4 4 3 3 4 4 1 0 0 0 0 3 1 1 0 1 0 0 0 0 0 "Undecided" 16 3 101 1 0 1 0 0 0 0 1 3 3 101 101 101 101 3 1 0 0 0 1 0 0 4 4 4 3 4 3 101 101 101 101 101 101 101 101 6 5 5 5 5 5 5 5 5 5 5 4 2 3 2 3 4 3 3 3 4 4 2 5 4 4 4 1 17 2 3 2 3 4 4 3 3 2 2 1)

  ,(Ans 6 4 3 3 4 4 4 4 101 4 4 3 3 3 4 2 2 2 2 2 2 2 2 3 2 1 0 0 0 0 3 1 1 7 0 0 1 0 0 0 "Business Administration" 15 3 2 0 0 0 0 1 0 0 1 6 3 101 101 101 101 3 0 0 0 0 0 0 0 3 4 101 2 1 1 3 3 101 2 1 1 101 101 6 5 5 5 3 3 1 1 1 2 1 1 3 3 3 3 3 3 3 3 3 3 3 2 1 2 2 1 17 1 1 1 1 4 3 3 1 2 2 2)

  ,(Ans 3 6 3 1 4 4 4 2 2 4 4 4 3 4 4 1 3 1 3 3 3 3 1 2 4 1 1 0 0 0 3 0 1 0 1 1 1 0 0 0 "Undecided" 16 3 101 1 0 0 0 0 0 0 1 3 3 101 101 101 101 3 1 1 0 0 0 0 0 4 3 3 3 4 3 3 4 3 2 4 3 101 101 6 5 4 4 4 5 3 5 3 4 4 5 1 3 1 3 3 3 3 2 2 2 1 3 3 3 4 1 17 2 4 4 101 101 101 101 101 101 101 101)

  ,(Ans 6 6 3 1 4 4 4 2 3 4 4 4 3 4 4 1 3 3 3 3 3 3 2 3 4 1 1 0 0 0 3 0 1 0 1 1 1 0 0 0 "Undecided" 16 3 101 1 0 0 0 0 0 0 1 3 3 1 1 1 2 3 1 1 0 0 0 0 0 4 4 3 3 4 3 3 3 3 3 4 3 101 101 6 5 5 5 5 5 5 5 3 5 4 4 1 1 1 3 3 2 3 2 2 2 1 3 3 3 4 1 17 2 4 4 3 4 3 3 3 2 2 1)

  ,(Ans 6 4 4 2 4 4 4 3 2 3 4 3 4 4 4 2 2 1 3 1 3 1 1 3 2 1 0 0 0 0 3 1 1 7 1 1 1 1 0 0 "Biology" 17 3 2 1 0 1 1 1 0 0 1 1 1 1 1 1 3 1 0 1 0 0 0 0 0 2 2 3 3 2 3 3 2 3 3 3 3 40 90 6 3 2 2 3 4 3 2 2 3 3 5 2 2 2 4 4 4 2 3 3 3 3 4 4 3 4 1 17 2 4 4 3 3 1 2 3 2 2 2)

  ,(Ans 6 2 4 2 4 4 4 4 1 4 4 4 4 4 4 1 3 2 4 1 4 1 1 2 2 1 0 0 0 1 4 1 1 7 0 0 0 0 0 1 "Administration of Justice" 17 3 2 1 0 0 1 0 0 0 1 3 1 3 1 1 1 2 1 1 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 6 15 6 3 4 2 3 5 5 5 5 5 3 5 4 5 2 2 2 5 5 4 4 4 4 4 5 5 4 1 17 2 3 1 3 3 3 3 3 2 2 1)

  ,(Ans 6 3 4 3 3 4 4 4 2 3 4 3 4 4 3 1 3 1 3 3 3 1 1 2 2 1 0 0 0 0 4 0 1 0 0 0 1 0 0 0 "Health Studies" 12 3 2 1 0 1 0 0 0 0 1 2 1 3 1 2 2 2 1 0 0 0 0 0 0 4 3 2 2 3 2 4 3 2 2 3 1 10 15 6 5 5 5 5 5 5 5 5 5 5 5 4 4 4 4 4 3 4 3 3 4 4 4 4 4 4 1 17 2 3 3 3 3 3 3 3 2 2 2)

  ,(Ans 6 2 3 2 4 4 4 4 2 3 3 3 1 3 2 3 3 1 3 3 2 1 1 2 3 1 0 0 0 0 4 1 1 7 0 0 0 0 0 1 "Biology" 15 2 2 0 0 0 0 0 0 1 1 3 1 2 1 1 3 1 1 1 0 0 0 0 0 4 3 3 4 4 3 4 2 3 2 3 3 14 30 5 3 3 2 3 3 3 2 2 3 0 2 3 3 2 4 5 3 4 3 3 2 2 5 5 4 5 1 17 1 2 3 3 3 4 3 3 1 0 0)

  ,(Ans 6 5 4 1 4 4 4 2 2 3 2 4 1 1 101 4 1 1 1 1 1 1 1 1 1 0 0 0 0 0 2 0 1 3 0 0 0 0 0 0 "Undecided" 17 2 101 1 0 1 1 0 0 0 1 1 1 2 1 1 3 1 0 0 0 0 0 0 0 4 4 4 3 4 3 3 3 3 1 4 2 5 5 6 5 5 3 5 5 3 3 4 4 1 5 3 3 2 3 3 3 2 3 3 2 2 3 4 4 5 1 17 2 5 5 3 4 101 3 3 2 2 2)

  ,(Ans 3 5 3 1 1 4 4 4 3 4 4 2 4 3 4 2 3 3 3 3 4 3 1 3 1 1 0 0 0 0 4 0 0 2 0 0 0 0 0 0 "Environmental Sciences and Resources" 14 2 101 1 0 0 1 1 0 0 1 1 2 3 2 1 1 1 1 1 0 0 0 0 0 3 3 3 3 3 3 2 2 3 2 2 1 6 10 6 4 3 3 5 3 4 2 1 2 1 2 4 4 3 4 4 5 1 2 3 3 4 4 4 4 5 0 17 2 5 5 3 3 3 3 3 2 2 2)

  ,(Ans 3 5 4 1 2 3 4 3 4 3 2 1 4 2 3 1 3 1 3 2 3 3 2 2 1 1 0 0 0 0 3 1 1 0 0 0 0 0 0 1 "Philosophy" 13 2 99 1 0 1 1 0 0 0 1 1 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 3 2 3 3 4 2 2 2 3 2 101 101 6 5 5 5 4 4 3 3 3 4 5 4 3 4 3 2 2 3 4 4 4 3 4 4 4 2 2 1 17 1 2 3 3 3 4 3 4 1 2 1)

  ,(Ans 6 3 3 1 4 4 4 4 2 4 4 3 4 3 1 1 2 1 2 1 3 1 3 4 3 1 0 0 0 0 3 0 0 0 0 0 1 0 0 0 "Psychology" 14 2 1 1 1 1 0 0 0 0 1 6 3 101 101 101 3 2 1 1 0 0 0 0 0 3 4 4 1 2 4 4 3 2 1 101 2 1 30 6 5 5 5 3 5 5 4 3 5 5 3 3 4 3 3 3 4 2 3 4 4 4 3 4 3 4 1 17 2 4 5 3 3 3 3 3 2 2 2)

  ,(Ans 6 6 3 1 2 4 4 3 2 2 3 3 4 3 2 1 3 1 3 2 3 2 1 3 2 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Undecided" 14 2 101 1 0 1 1 0 0 0 1 6 3 101 101 2 101 3 1 1 0 0 0 1 1 4 3 3 2 2 4 4 2 3 2 3 4 101 101 6 4 5 101 5 5 4 5 3 5 4 5 2 4 4 3 4 4 2 3 4 3 4 3 4 4 4 1 17 2 4 5 3 4 4 3 3 1 2 1)

  ,(Ans 6 3 3 3 4 4 4 4 3 4 4 3 4 3 4 1 3 1 4 2 4 3 3 4 1 1 0 0 0 0 2 1 1 0 0 0 1 0 0 0 "Business Administration" 14 2 2 1 1 1 1 1 0 0 1 1 3 2 2 1 2 2 1 1 0 0 0 0 0 3 4 4 2 3 3 4 4 4 1 3 3 2 8 6 5 5 5 5 5 5 5 4 5 4 3 3 3 2 3 3 2 3 2 3 3 3 3 4 4 4 1 17 2 3 5 3 3 3 3 3 2 2 2)

  ,(Ans 6 2 4 3 3 3 4 4 3 4 4 3 3 3 3 4 3 1 3 3 3 2 3 1 2 1 0 0 1 0 5 1 1 0 0 0 0 0 0 0 "Biology" 15 2 101 1 0 1 0 1 0 0 1 1 1 2 2 1 3 1 0 1 0 0 0 0 0 4 4 4 4 4 4 4 2 3 3 4 4 12 40 6 5 5 4 0 3 5 5 3 2 1 2 3 2 3 3 3 4 2 2 2 3 2 3 4 2 2 0 17 2 2 5 3 3 4 3 3 2 2 2)

  ,(Ans 6 5 4 1 3 3 4 3 4 4 3 4 2 3 2 1 1 1 3 1 1 3 3 3 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Undecided" 15 2 1 1 0 1 1 0 0 0 1 6 3 1 1 2 2 3 0 1 0 0 0 0 1 4 2 2 1 3 2 3 2 3 1 2 1 101 101 6 5 5 5 5 5 5 5 5 5 4 5 5 5 3 3 4 4 4 5 5 4 5 4 3 2 2 1 17 2 3 2 3 4 3 3 3 3 2 2)

  ,(Ans 6 6 3 2 4 3 3 1 2 4 2 4 2 4 2 1 3 1 3 1 3 1 1 3 4 1 0 0 0 0 4 1 1 0 0 1 0 0 0 0 "Civil Engineering" 16 1 101 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 1 0 0 0 0 0 0 4 3 3 2 3 1 4 4 2 1 3 1 101 101 6 5 5 5 5 5 2 4 1 5 2 5 2 3 3 3 3 2 3 2 3 2 3 3 3 3 3 1 17 1 3 3 3 4 4 3 3 2 2 2)

  ,(Ans 3 2 3 1 2 3 3 4 2 2 4 4 4 4 3 1 3 1 3 1 4 1 2 4 4 1 0 0 0 0 4 1 1 4 0 0 0 0 0 1 "Did not answer" 15 1 99 0 0 0 0 0 0 1 1 6 3 1 1 1 1 3 1 1 0 0 0 0 1 4 3 4 2 4 3 3 2 3 3 4 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 3 4 4 4 4 5 3 4 4 3 5 3 4 4 3 1 17 2 1 5 1 3 4 2 4 2 2 2)

  ,(Ans 3 3 4 1 4 4 4 3 2 4 4 3 4 4 3 1 4 1 3 3 3 4 1 3 1 1 1 0 0 0 4 1 1 7 0 0 0 0 1 0 "Biology" 15 1 101 0 0 1 0 0 0 1 1 1 3 101 101 101 101 3 0 0 0 0 0 0 0 3 1 1 1 3 1 2 1 1 1 2 1 101 101 6 5 5 4 5 5 2 5 2 5 5 2 4 5 5 4 5 2 2 5 5 3 5 3 4 4 5 1 17 2 5 5 3 3 2 3 3 2 2 3)

  ,(Ans 6 2 4 4 4 4 4 3 3 3 4 2 2 4 2 1 3 4 4 2 3 3 2 4 3 1 0 0 0 0 4 1 0 0 0 1 0 0 0 1 "Undecided" 13 1 101 1 1 0 1 1 0 0 1 6 3 1 1 1 1 3 1 0 0 0 1 0 0 4 2 2 2 2 3 3 2 2 3 2 3 101 101 6 5 5 5 2 4 3 2 1 5 4 5 3 3 3 5 5 4 5 2 3 5 4 4 5 5 5 1 17 1 2 5 3 3 3 3 3 3 0 3)

  ,(Ans 6 3 4 2 2 3 4 4 3 3 3 3 2 3 4 1 3 2 3 1 2 3 3 4 2 1 0 0 0 0 4 1 1 0 1 0 0 0 0 0 "International Studies" 13 1 101 1 0 1 0 0 0 0 1 1 3 1 1 1 2 3 1 0 0 0 1 0 0 1 2 2 3 101 101 3 2 2 3 1 3 101 101 6 4 4 5 5 5 5 5 4 4 3 5 3 3 2 4 4 5 4 2 2 2 3 4 3 3 3 1 17 2 2 2 3 2 2 2 3 2 2 2)

  ,(Ans 6 3 4 4 4 4 4 4 3 4 4 4 2 3 3 1 3 4 4 4 4 2 1 4 4 0 0 0 0 0 3 1 1 0 0 0 0 0 0 0 "International Studies" 13 1 101 1 0 1 0 1 0 0 1 3 3 1 1 2 1 3 0 0 0 0 0 1 0 4 4 4 4 101 4 4 3 4 2 4 3 101 101 6 5 5 5 5 5 5 5 5 5 5 5 4 5 4 5 4 5 5 4 5 5 5 5 5 5 5 1 17 2 2 4 3 3 4 3 3 2 2 2)

  ,(Ans 3 4 3 2 2 3 3 3 1 2 2 3 2 2 1 1 2 1 2 1 2 1 1 3 1 1 0 0 0 0 4 1 1 99 0 0 0 0 0 1 "Civil Engineering" 18 1 101 1 0 0 1 1 0 0 1 3 3 1 1 1 2 3 0 0 0 0 1 0 1 4 4 4 3 3 3 4 3 3 3 3 2 101 101 5 0 5 0 3 2 0 1 1 3 2 5 1 1 1 2 2 2 2 2 2 3 2 5 5 4 4 0 17 1 4 5 3 4 4 3 3 2 2 2)

  ,(Ans 6 6 4 1 1 3 4 3 4 4 4 3 3 3 3 1 3 4 2 3 3 1 1 1 4 0 0 0 1 0 3 1 1 7 0 0 0 0 0 0 "Undecided" 101 1 101 1 0 0 0 0 0 0 1 3 3 2 2 1 2 3 0 0 0 0 0 0 0 101 101 101 101 101 101 3 4 101 2 3 2 101 101 6 4 4 3 0 3 4 3 4 4 1 3 3 3 3 3 3 3 3 2 3 1 3 3 3 3 3 1 17 1 2 2 3 4 4 3 3 2 101 3)

  ,(Ans 6 3 4 1 1 3 4 4 3 4 4 3 3 3 3 4 4 1 3 1 3 3 2 3 4 1 0 0 0 0 4 0 0 0 1 1 0 1 0 0 "Undecided" 13 1 101 1 0 1 0 0 0 0 1 3 1 1 1 1 3 1 0 0 0 0 0 0 0 2 3 3 4 2 1 2 3 3 4 2 1 15 60 6 3 5 2 3 1 2 1 0 2 1 1 1 3 3 3 3 3 1 1 2 2 2 3 2 2 3 1 17 2 4 99 3 4 4 2 3 3 2 3)

  ,(Ans 6 2 4 1 3 4 4 4 3 4 4 3 4 3 4 1 4 1 4 3 3 2 3 4 3 1 0 0 0 0 4 1 1 0 1 1 1 1 0 0 "Undecided" 15 1 101 1 0 1 1 1 0 0 1 2 3 1 1 1 1 3 1 1 1 0 0 0 0 4 3 4 2 3 4 4 4 4 3 2 4 101 101 6 3 5 5 2 4 3 4 5 5 1 3 3 3 3 4 4 3 2 3 5 4 4 5 5 3 3 0 17 2 5 5 3 3 4 3 3 2 2 2)

  ,(Ans 3 2 3 3 3 4 4 3 3 3 3 4 2 3 2 1 2 2 3 2 3 1 1 2 3 1 0 0 0 0 2 1 0 7 0 1 0 0 0 1 "Biology" 14 1 101 1 0 0 0 0 0 0 1 1 3 1 2 1 2 3 1 0 0 0 0 0 1 3 3 4 2 2 2 3 2 3 3 3 2 101 101 6 5 4 4 5 5 5 5 4 4 4 5 2 2 2 4 4 4 3 3 3 4 4 4 4 4 4 1 17 1 99 99 3 2 2 3 3 2 2 2)

  ,(Ans 3 3 3 1 4 4 4 4 4 4 4 3 3 3 2 3 2 3 3 1 2 3 2 3 4 1 0 0 0 0 4 1 1 0 0 0 0 0 0 1 "Computer Engineering" 13 1 101 1 0 1 1 0 0 0 1 3 1 3 2 1 1 1 1 0 0 0 0 0 0 3 4 3 2 4 3 3 4 1 1 3 2 6 15 6 5 5 2 5 5 5 5 3 5 5 5 3 4 3 5 4 4 3 2 3 5 4 4 5 3 4 1 17 1 2 3 3 3 4 3 3 1 2 2)

  ,(Ans 6 1 3 1 4 4 4 4 3 4 3 2 2 2 3 1 3 4 3 3 3 2 1 3 2 1 0 0 0 0 4 0 1 0 0 0 0 0 0 1 "Business Administration" 13 1 99 1 0 1 1 1 0 0 1 3 3 1 1 1 3 3 0 0 0 0 1 0 0 4 3 3 4 3 3 4 3 3 4 3 3 101 101 6 5 5 5 5 2 1 2 2 3 2 2 3 3 4 4 4 3 3 3 3 3 4 4 4 4 4 1 17 2 3 2 3 3 4 3 3 0 0 0)

  ,(Ans 6 4 3 3 3 3 4 4 1 3 2 2 2 3 3 1 3 2 3 3 2 2 1 1 1 0 0 0 0 1 4 1 1 1 0 0 1 0 1 0 "Computer Engineering" 14 1 101 1 0 1 1 1 0 0 1 3 3 1 1 1 2 3 1 0 0 0 0 0 1 3 4 4 4 3 2 3 4 4 4 2 2 101 101 6 0 5 0 3 5 3 0 0 3 0 0 1 2 2 4 4 5 3 3 3 2 2 4 3 2 2 0 17 1 4 3 3 4 4 3 3 2 0 0)

  ,(Ans 6 2 4 1 4 4 4 4 1 4 3 4 3 4 4 4 3 3 4 4 3 1 1 2 3 1 0 0 0 0 2 1 1 2 0 0 1 0 0 0 "Undecided" 14 1 2 1 0 1 0 0 0 0 1 1 1 2 2 1 2 1 0 0 0 0 0 0 0 101 101 101 101 101 101 3 1 3 2 1 3 150 40 6 4 4 3 0 5 4 2 3 1 0 5 3 3 4 3 3 3 3 3 3 3 3 4 4 4 3 1 17 2 1 4 3 4 4 3 3 2 2 3)

  ,(Ans 101 3 4 4 4 4 4 4 1 4 2 2 3 4 2 1 3 1 3 1 3 1 3 4 2 1 0 0 0 0 4 1 1 0 0 0 1 0 1 0 "Did not answer" 15 1 101 1 0 0 0 0 0 0 1 6 3 1 1 2 1 3 0 0 0 0 0 0 1 4 2 1 2 1 1 4 1 1 2 1 1 101 101 6 5 5 5 3 3 5 5 5 5 5 4 5 5 4 4 4 5 5 5 5 3 4 3 5 1 1 0 17 1 4 4 3 1 1 3 3 1 0 1)

  ,(Ans 99 5 4 4 4 4 4 4 3 4 2 4 2 3 3 1 2 3 3 3 3 3 1 1 4 0 0 0 1 0 4 0 1 7 0 0 1 0 0 0 "Undecided" 12 1 2 1 0 1 0 1 0 1 1 2 101 101 101 101 2 3 1 0 0 0 1 1 0 4 4 2 3 3 2 4 4 2 3 3 2 101 101 6 2 2 2 4 3 1 2 0 0 0 3 2 2 2 4 3 1 2 1 3 3 2 3 4 1 4 1 17 1 5 99 3 1 2 3 3 2 2 2)

  ,(Ans 6 3 4 1 4 4 4 3 101 4 4 3 1 4 4 1 4 4 4 4 4 1 1 1 3 0 0 0 1 0 4 1 1 99 0 0 0 0 0 1 "Accounting" 13 1 99 1 0 1 0 0 0 0 1 3 3 1 1 1 1 3 0 0 0 0 1 0 0 4 3 4 3 4 2 4 2 4 3 3 1 101 101 6 5 3 0 5 3 2 2 2 1 3 1 3 3 2 5 5 5 3 4 3 4 3 4 4 4 4 1 17 2 4 4 3 3 4 3 3 2 2 2)

  ,(Ans 3 6 3 4 4 4 3 2 3 4 4 4 3 4 3 4 3 1 3 3 3 2 2 4 4 1 0 0 0 0 4 1 1 0 0 0 1 0 0 0 "Computer Science" 14 2 2 1 0 1 1 1 0 0 1 2 1 2 1 1 3 1 0 0 0 0 0 0 0 4 3 3 3 3 3 4 3 2 2 1 3 10 35 6 5 5 5 1 5 5 5 3 5 2 3 3 3 3 3 3 4 1 3 4 2 3 4 4 3 3 1 17 101 101 101 101 101 101 101 101 101 101 101)

  ]