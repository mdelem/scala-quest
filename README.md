# Scala Quest
A questionnaire library for scientific and marketing purposes.
For now, mostly an excuse to explore Scala features.

Other than the goal of learning, this project aims to provide a reusable and very flexible server-side library to define and run questionnaires.
- The questionnaire definitions (content and order of parts, questions and items) should be easy to edit as this is often done by a non-technical person.
- The questionnaire branching and response handling should have sensible defaults but at the same time be extremely flexible as there are always special cases (custom API calls, authentication and authorization, conversions to relational data, etc). This would be the job of the Scala developer.


## What I learned
### The good
- The 'Action' API that handles questionnaire progress, branching and response storage is solid. It could probably be used as is in production.

### The improvable
- I abused implicits to create a nice questionnaire-building API so that questionnaire definitions are checked at compile time. This is pretty nice but I'm considering switching to a parsed DSL so that end-users without any Scala experience can use it. The auto-indentation doesn't make things too beautiful either. Or I could just use JSON as questionnaire definitions.
- Macros makes you feel like a wizard but are a pain for maintenance / refactoring. I used them to improve type-safety but with mixed results.
- Scala's type system enables you to do a lot a secure things... but still gets in the way sometimes. Especially when writing a questionnaire library that needs a lot of flexibility for all the possible questions/answer types. Sometimes I feel that I should be using a String type everywhere. Maybe one day I'll try to rewrite the project in a dynamic language to see how that goes.
