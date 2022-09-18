# Financial planner

### Commands
- list - list of spendings, parameters: [count optional]
- clear - clear console
- createExpected - create expected spending, parameters: [estimatedCost **required**, expenditureObject **required**]
- makeActual - make actual expected spendings, parameters: [spendDate **required**, spendingId **required**, actualCost **required**]
- shortStats - short statistics about spendings
- delete - delete spendings, parameters: [spendingId **required**]

### Parameters
- count - example -count:5
- estimatedCost - example: -estimatedCost:"40000r"
- expenditureObject - example: -expenditureObject:"Кресло ортопедическое"
- actualCost - example: -actualCost:"40000r", where r is currency ruble
- spendDate - example: -spendDate:"10.05.2002"
- spendingId - example: -spendingId:"da0b7e63-62ea-4d65-8a13-394455fa6bf2"

Example: createExpected -estimatedCost:"40000r" -expenditureObject:"Кресло ортопедическое"
