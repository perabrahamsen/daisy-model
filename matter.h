// matter.h

#ifndef MATTER_H
#define MATTER_H

class AttributeList;
class Syntax;
class OrganicMatter;
class InorganicMatter;
class SoluteMatter;

extern const OrganicMatter& MakeOrganicMatter (const AttributeList&);
extern const InorganicMatter& MakeInorganicMatter (const AttributeList&);
extern const SoluteMatter& MakeSoluteMatter (const AttributeList&);

extern const Syntax& OrganicMatterSyntax ();
extern const Syntax& InorganicMatterSyntax ();
extern const Syntax& SoluteMatterSyntax ();

extern AttributeList& OrganicMatterAlist ();
extern AttributeList& InorganicMatterAlist ();
extern AttributeList& SoluteMatterAlist ();

#endif MATTER_H
