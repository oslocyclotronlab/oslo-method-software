int offline_clear( )
{
	
        const char 	*startp = "/user/schiller/osloware/bin/offline_specclear &";
	char		msg[1024] = "Spectrum area in shared memory zeroed";
	char		err[1024] = "*** ERROR *** Could not start offline_specclear";


        if ( system( startp ) == -1) {
           errprint("%s\n",err);
           return -1;
        }  

	wprint("%s\n",msg);


        return 0;
}
