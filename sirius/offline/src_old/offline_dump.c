int offline_dump( )
{
	const char	*startp = "/Applications/sirius/bin/offline_specdump &";
	char	msg[1024]	= "Dumping spectra to disk files ...";
	char	err[1024]	= "*** ERROR *** Could not start offline_specdump";

	if ( system( startp ) == -1) {
		errprint("%s\n",err);
		return -1;
	} 
	wprint("%s\n",msg);
	return 0;
}
