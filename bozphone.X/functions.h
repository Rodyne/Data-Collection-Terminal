#ifndef FUNCTIONS_H
#define	FUNCTIONS_H

uint8  StrLen(char *str); // get the length of string
char   StrUCase(char x); // convert input char to uppercase (for comparason below)
uint8  StrCpySubStr(char *substr, char *str, uint8 StartPos, uint8 NumChars); // copies str into substring from pos start for numchars or until null or \r
uint8  StrLocate(char *SubStr, char *str); // see if substr is in str and return 0 if false (case insensitive) or position of substr if true

#endif	/* FUNCTIONS_H */

