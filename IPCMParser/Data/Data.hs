module Data.Data(
Organism,
organismIDMap,
RuleOperator,
matchOperator,
SearchParameter,
searchParameterList
)
  where
    import Data.Map.Strict

    type Organism = String
    organismIDMap :: [(Organism, Int)]
    organismIDMap = [
    ("MRSA",6), ("CDT", 7), ("ESBL", 8), ("Tuberculosis", 9), ("VRE", 10), ("Group A streptococcus", 11),
    ("Norovirus", 12), ("Adenovirus", 13), ("Rotavirus", 14), ("H1N1", 15), ("Varicella zoster virus", 16),
    ("MRSA bacteraemia", 17), ("MSSA", 22), ("E.coli", 23), ("Campylobacter", 24), ("Shigella", 25), ("Salmonella", 26),
    ("RSV", 27), ("Gentamicin resistant organisms", 28), ("CD-GDH", 29), ("Influenza", 34), ("CPE", 35),
    ("MRSA Negative", 37), ("AMPC", 38)]

    type RuleOperator :: String
    matchOperator :: [MatchOperator]
    matchOperator = [
    "And", "Or", "Is", "Is not", "Includes", "Excludes", "Significant isolates", "Non-significant isolates"] --Translate to Like and NotLike

    type SearchParameter = String
    searchParameterMap :: [(SearchParameter, String)]
    searchParameterMap = [
    ("Test name", "TestName"), ("Text text", "TestText"), ("Specimen", "SpecimenType"),
    ("Investigation", "InvestigationType;InvestigationDescription")]
