int acq_dump( )
{
	const char	*startp = "/Applications/sirius/bin/acq_specdump &";
	char		msg[1024] = "Dumping spectra to disk files ...";
	char		err[1024] = "*** ERROR *** Could not start acq_specdump";

	if ( system( startp ) == -1) {
		errprint("%s\n",err);
		return -1;
	} 
	wprint("%s\n",msg);
	return 0;
}
