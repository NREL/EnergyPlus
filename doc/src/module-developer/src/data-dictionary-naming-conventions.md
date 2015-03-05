# Data Dictionary Naming Conventions

## Class (Object) Names

Class names shall be written in camel case where words are joined together without spaces and each word is capitalized. The colon shall be used to delimit hierarchy moving from general to specific. Natural language terminology shall be used when possible for each member separated by colons.

~~~~~~~~~~~~~~~~~~~~

    Example: ZoneHVAC:PackagedTerminalHeatPump
~~~~~~~~~~~~~~~~~~~~

## Field Names

Field names shall be written in title case where every major word is capitalized (exceptions: "a", "the", "for", etc.) with spaces separating words.  Field names shall be written using natural language terminology but should be relatively concise (no unnecessary abbreviations or acronyms).  If the field is for the name of this object, the field name shall simply be "Name" to eliminate redundancy.  The nature of an alpha field shall be explicit, for instance, "Availability Schedule Name" instead of merely "Availability Schedule". Generally, the object is not also included as the field name.

~~~~~~~~~~~~~~~~~~~~

    Example: Availability Schedule Name
~~~~~~~~~~~~~~~~~~~~

When object names/types are included as part of the input, then the field name should contain "Object Type":

~~~~~~~~~~~~~~~~~~~~

    Example: Zone Equipment 1 Object Type
~~~~~~~~~~~~~~~~~~~~

## Choice Names

When field choices list object references, the field choices shall list the class/object type name the same as its class definition in the IDD.  When field choices list other key words, the field choice names shall be written in camel case where words are joined together without spaces and each word is capitalized.  Field choice names shall be written using natural language terminology but should be relatively concise (no unnecessary abbreviations or acronyms) and avoid overly lengthy key words.  Field choice names shall only use alphanumeric characters with the addition of the forward slash ("/") character as a concise alternative to the word "per"; colons shall be allowed if the field choices are class names.

~~~~~~~~~~~~~~~~~~~~

      A2 , \field Period Selection
           \retaincase
           \note Following is a list of all possible types of Extreme and Typical periods that
           \note might be identified in the Weather File. Not all possible types are available
           \note for all weather files.
           \type choice
           \key SummerExtreme
           \key SummerTypical
           \key WinterExtreme
           \key WinterTypical
           \key AutumnTypical
           \key SpringTypical
           \key WetSeason
           \key DrySeason
           \key NoDrySeason
           \key NoWetSeason
           \key TropicalHot
           \key TropicalCold
~~~~~~~~~~~~~~~~~~~~