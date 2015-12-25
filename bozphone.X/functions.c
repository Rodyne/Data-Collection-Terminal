#include "hardware.h"
#include "functions.h"

uint8 StrLen(char *str) // get the length of string
{
	uint8 length=0;
	while(*(str+length)!='\0')
		length++; // get to start of string where we want to copy from
	return length;
}

char StrUCase(char x) // convert input char to uppercase (for comparason below)
{
	if(x>96 && x<123) // conv a..z to A..Z
		return x-32;
	else
		return x;
}

uint8 StrCpySubStr(char *substr, char *str, uint8 StartPos, uint8 NumChars) // copies str into substring from pos start for numchars or until null or \r
{
	*substr = '\0'; // clr substring;
	while(*str!='\0' && StartPos--)
		str++; // get to start of string where we want to copy from
	if(*str=='\0') return 0; // if we got to the end of the str but not to the posn we want there is an error
	while(*str!='\0' && *str!='\r' && NumChars--) // Add s1 to end until we have counted numchars to 0 (NOTE we also terminate on \r!)
	{
		*substr++ = *str++;
	}
	*substr='\0';
	if(NumChars) return 0; else return 1; // return 1 if ok and 0 if we could not copy all the chars
}

uint8 StrLocate(char *SubStr, char *str) // see if substr is in str and return 0 if false (case insensitive) or position of substr if true
{
	uint16 i=0;
	char *OrigMatchStr = SubStr; // save the original address of the string in case we dont get a match and need to reset
	while(*str!='\0')
	{
		if( StrUCase(*SubStr) == StrUCase(*str++))
		{
			SubStr++;
			if(*SubStr=='\0')	return i;
		}
		else
			SubStr = OrigMatchStr; //reset search
		i++;
	}
	return 0;
}
