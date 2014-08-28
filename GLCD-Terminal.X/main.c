/* main loop of the GLCD Data collection terminal
 *
 * Note: if using mplabx then only add this file to the 'source files' of the project and keep the other files of the project in
 * the 'important files' alternatively put them in the 'source files' part but right click on their properties and exclude them
 * from the build. The reason for this is when you build the project it will compile source modules regardless of any ifndef and you
 * will end up with multiple definitions. There is probably a proper way to do this using headers if there is then please do it correctly
 * this works for me until somebody puts me right :-)
 */

#include "parser.c"
#include "helper_funcs.c"

int main()
{
	InitTerminal();

	DoLogo();

	WiFiInit(1);

	if(!WIFI_LINK()) SetupWIFI();

	while(1)
	{
	  InitBarcodeScan();
  	run();
	}
}

