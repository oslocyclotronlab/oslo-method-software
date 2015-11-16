int grabchar_()
{
  int c;

/*  system("/bin/stty icanon"); */
  c = getchar();
/*  system("/bin/stty -icanon");*/
  return ( c );
}

