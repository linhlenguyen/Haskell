module IPCRule(
toString
)
  where
    import Data.Data

    data IPCRule = IPCRule {
        parent_rule_id :: IPCRule,
        organism_id :: Int,
        rule_type_id :: Int,
        rule_operator :: RuleOperator,
        parameter_name :: SearchParameter,
        parameter_value :: String,
        template :: IPCRule,
        template_input :: String
    }

    parseOrganism :: String -> [IPCRule]
    parseOrganism = undefined

    parseOperator :: String -> [IPCRule]
    parseOperator = undefined

    --Example String
    --Specimen is faeces
    --Significant isolates Mycobacterium tuberculosis ISOLATED
    --Test text includes C.difficile Toxin A & B
    --Test name is Culture
    parseRule :: String -> [IPCRule]
    parseRule = undefined
