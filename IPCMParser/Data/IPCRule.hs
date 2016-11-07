{-# LANGUAGE GADTs #-}

module IPCRule(
toXML
)
  where
    import Data.Data

    type RuleOperator = String
    type SearchParameter = String
    type Organism = String

    data IPCRule = IPCRule {
        rule_id :: Int,
        parent_rule_id :: Maybe IPCRule,
        organism_id :: Maybe Int,
        rule_type_id :: Int,
        rule_operator :: Maybe RuleOperator,
        parameter_name :: Maybe SearchParameter,
        parameter_value :: Maybe String,
        template :: Maybe IPCRule,
        template_input :: Maybe String
    }

    --Store IPC states
    data IPCConfiguration = IPCConfiguration{
        trust_code :: String,
        significant_isolate :: IPCRule,
        non_significant_isolate :: IPCRule,
        rules :: [IPCRule]
    }

    data Tree a where
      Empty :: Tree a
      Cons :: a -> [Tree a] -> Tree a

    toXML :: IPCRule -> String
    toXML r = "<IPC_Rule>" ++ ruleId ++ parentRuleId ++ organismID ++ ruleTypeId ++ ruleOperator ++ parameterName ++ parameterValue ++ templateID ++ templateInput ++ "</IPC_Rule>"
      where
        ruleId = "<rule id type=\"Int32\">" ++ show rule_id r ++ "</rule_id>"
        parentRuleId = case parent_rule_id r of Nothing -> "<parent_rule_id type=\"Int32\"xsi:nil=\"true\"/>"
                                                Just a -> "<parent_rule_id type=\"Int32\">" ++ show a ++ "</parent_rule_id>"
        organism_id = case organism_id r of Nothing -> "<organism_id type=\"Int32\">xsi:nil=\"true\" />"
                                            Just a -> "<organism_id type=\"Int32\">" ++ show a ++ "</organism_id>"
        ruleTypeId = "<rule_type_id type=\"Int32\">" ++ rule_type_id r ++ "</rule_type_id>"
        ruleOperator = case rule_operator r of Nothing -> "<rule_operator type=\"String\"xsi:nil=\"true\"/>"
                                               Just a -> "<rule_operator type=\"String\">" ++ a ++ "</rule_operator>"
        parameterName = case parameter_name r of Nothing -> "<parameter_name type=\"String\"xsi:nil=\"true\"/>"
                                                 Just a -> "<parameter_name type=\"String\">" ++ searchParameterMap!a ++ "</parameter_name>"
        parameterValue = case parameter_value r of Nothing -> "<parameter_value type=\"String\"xsi:nil=\"true\"/>"
                                                   Just a -> "<parameter_value type=\"String\">" ++ a ++ "</parameter_value>"
        templateID = case template r of Nothing -> "<template_id type=\"Int32\">xsi:nil=\"true\" />"
                                        Just a -> "<template_id type=\"Int32\">" ++ show a ++ "</template_id>"
        templateInput = case template_input r of Nothing -> "<template_input type=\"String\"xsi:nil=\"true\"/>"
                                                 Just a -> "<template_input type=\"String\">" ++ a ++ "</template_input>"
