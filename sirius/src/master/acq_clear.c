int acq_clear( )
{
	const char 	*startp = "/Applications/sirius/bin/acq_specclear &";
	char		msg[1024] = "Spectrum area in shared memory zeroed";
	char		err[1024] = "*** ERROR *** Could not start acq_specclear";

	if ( system( startp ) == -1) {
		errprint("%s\n",err);
		return -1;
	}  
	wprint("%s\n",msg);
	return 0;
}
