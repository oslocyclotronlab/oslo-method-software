#include <stdio.h>
#include <math.h>
#include <stdlib.h>

main()
{
   int cmd;
   void hd();
   void dh();
   void ho();
   void oh();
   void hb();
   void bh();
   void menu();

   printf("\n");
   printf("  ________________________________________ \r\n");
   printf(" |                                        |\r\n");
   printf(" |               H e x  1.3               |\r\n");
   printf(" |                                        |\r\n");
   printf(" |      Program to translate numbers      |\r\n");
   printf(" |     between various number systems     |\r\n");
   printf(" |             Hex  <-->  Dec             |\r\n");
   printf(" |             Hex  <-->  Oct             |\r\n");
   printf(" |             Hex  <-->  Bin             |\r\n");
   printf(" |                                        |\r\n");
   printf(" | E-mail  : magne.guttormsen@fys.uio.no  |\r\n");
   printf(" | Created : 04-02-1998                   |\r\n");
   printf(" | Modified: 25-09-1998                   |\r\n");
   printf(" |________________________________________|\r\n");
   printf("                                           \r\n");

   menu();
   printf("hex>");
   do {
      cmd = readchar();
      switch (cmd)
      {
      case 'a' :hd(); break;
      case 'b' :dh(); break;
      case 'c' :ho(); break;
      case 'd' :oh(); break;
      case 'e' :hb(); break;
      case 'f' :bh(); break;
      case '\n':{printf("hex>");break;}
      case '*' :break;
      default  :{printf("Illegal command, try again\n");printf("hex>");}
      }
   }while (cmd != '*');
   return 0;
}

void menu() {
   printf(" a : Hex -> Dec \r\n");
   printf(" b : Dec -> Hex \r\n");
   printf(" c : Hex -> Oct \r\n");
   printf(" d : Oct -> Hex \r\n");
   printf(" e : Hex -> Bin \r\n");
   printf(" f : Bin -> Hex \r\n");
   printf(" * : Exit       \r\n");
   return;
}

void hd() {
   unsigned long int i;
   printf("Type your Hex-number: "  );
   scanf("%Lx",&i);
   printf("Hex -> Dec          : %Lu\n",i);
   menu();
   return;
}

void dh() {
   unsigned long int i;
   printf("Type your Dec-number: "  );
   scanf("%Lu",&i);
   printf("Dec -> Hex          : %Lx\n",i);
   menu();
   return;
}

void ho() {
   unsigned long int i;
   printf("Type your Hex-number: "  );
   scanf("%lx",&i);
   printf("Hex -> Oct          : %Lo\n",i);
   menu();
   return;
}

void oh() {
   unsigned long int i;
   printf("Type your Oct-number: "  );
   scanf("%lo",&i);
   printf("Oct -> Hex          : %Lx\n",i);
   menu();
   return;
}

void hb() {
   int k;
   unsigned long int u, mask[8];
   unsigned char v;

   mask[0]=0x0000000f;
   mask[1]=0x000000f0;
   mask[2]=0x00000f00;
   mask[3]=0x0000f000;
   mask[4]=0x000f0000;
   mask[5]=0x00f00000;
   mask[6]=0x0f000000;
   mask[7]=0xf0000000;

   printf("Type your Hex-number: "  );
      scanf("%lx",&u);
      printf("Hex -> Bin          : ");
      for(k=0;k<8;k++){
         v = (unsigned char)((u & mask[7-k])>>((7-k)*4));
         if(v == 0x0)printf("0000 ");
         if(v == 0x1)printf("0001 ");
         if(v == 0x2)printf("0010 ");
         if(v == 0x3)printf("0011 ");
         if(v == 0x4)printf("0100 ");
         if(v == 0x5)printf("0101 ");
         if(v == 0x6)printf("0110 ");
         if(v == 0x7)printf("0111 ");
         if(v == 0x8)printf("1000 ");
         if(v == 0x9)printf("1001 ");
         if(v == 0xa)printf("1010 ");
         if(v == 0xb)printf("1011 ");
         if(v == 0xc)printf("1100 ");
         if(v == 0xd)printf("1101 ");
         if(v == 0xe)printf("1110 ");
         if(v == 0xf)printf("1111 ");
      }    
   printf("\n");
   menu();
   return;
}

void bh() {
   unsigned long int u;
   char s1[50], *end;
   printf("Type your Bin-number: "  );
   scanf("%s",s1);
   u = strtoul(s1, &end, 2);
   printf("Bin -> Hex          : %lx\n",u);
   menu();
   return;
}

int readchar() {
   int c1;
   int c2 = '\n';
   while((c1 = getchar()) != '\n')
      c2 = tolower(c1);
   return c2;
}
