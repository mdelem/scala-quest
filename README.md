# scala-quest
A questionnaire library for scientific and marketing purposes. For now, mostly an excuse to explore Scala features.

## What I learned
### The good
- The 'Action' API that handles questionnaire progress, branching and response storage is solid. It could probably be used as is in production.
### The improvable
- I abused implicits to create a nice questionnaire-builing API so that questionnaire definitions are checked at compile time. This is pretty nice but I'm considering switching to a parsed DSL so that end-users without any Scala experience can use it. The auto-indentation doesn't make things too beautiful either. Or I could just use JSON as questionnaire definitions.
- Macros makes you feel like a wizard but are a pain for maintenance / refactoring. I used them to improve type-safety but with mixed results.
- Scala's type system enables you to do a lot a secure things... but still gets in the way sometimes. Especially when writing a questionnaire library that needs a lot of flexibility for all the possible questions/answer types. Sometimes I feel that I should be using a String type everywhere. Maybe one day I'll try to rewrite the project in a dynamic language to see how that goes.
