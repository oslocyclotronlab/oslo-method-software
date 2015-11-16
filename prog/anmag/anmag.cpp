{
	gROOT->Reset();
	gROOT->SetStyle("Plain");
	gStyle->SetOptTitle(0);
	gStyle.SetOptStat(0);
    
	m = (TH1F*)gROOT->FindObject("h");
	if (m) m->Delete();
	TCanvas *c1 = new TCanvas("c1","Current as function of beam energy",700,500);
        
    TH2F *h = new TH2F("h"," ",100,2.0,45,100,20,135);

	int   i;
	float Ti, B1i, I1i, B2i, I2i, B3i, I3i, B4i, I4i;
    float T[500], I1[500], I2[500], I3[500], I4[500];
    float Texp[100],Iexp[100],dTexp[100],dIexp[100];

///////////////////////////////////////////////////////
    ifstream anmag_out("anmag_out.txt");
    i=0;
	while(anmag_out){
		anmag_out >> Ti >> B1i >> I1i >> B2i >> I2i >> B3i >> I3i >> B4i >> I4i;
        T[i]=Ti;
        I1[i]=I1i;
        I2[i]=I2i;
        I3[i]=I3i;
        I4[i]=I4i;
		i++;
	}
    TGraph *pteo   = new TGraph(500,T,I1);
    TGraph *dteo   = new TGraph(500,T,I2);
    TGraph *he3teo = new TGraph(500,T,I3);
    TGraph *he4teo = new TGraph(500,T,I4);

///////////////////////////////////////////////////////
    ifstream p("settings_p.txt");
    i=0;
	while(p){
		p >> Ti >> I1i;
        Texp[i]=Ti;
        Iexp[i]=I1i;
        dTexp[i]=0.05;
        dIexp[i]=0.05;
        i++;
	}
    TGraphErrors *pexp   = new TGraphErrors(i,Texp,Iexp,dTexp,dIexp);
    
    ifstream d("settings_d.txt");
    i=0;
	while(d){
		d >> Ti >> I1i;
        Texp[i]=Ti;
        Iexp[i]=I1i;
        dTexp[i]=0.05;
        dIexp[i]=0.05;
        i++;
	}
    TGraphErrors *dexp   = new TGraphErrors(i,Texp,Iexp,dTexp,dIexp);

    ifstream he3("settings_3he.txt");
    i=0;
	while(he3){
		he3 >> Ti >> I1i;
        Texp[i]=Ti;
        Iexp[i]=I1i;
        dTexp[i]=0.05;
        dIexp[i]=0.05;
        i++;
	}
    TGraphErrors *he3exp   = new TGraphErrors(i,Texp,Iexp,dTexp,dIexp);

    ifstream he4("settings_4he.txt");
    i=0;
	while(he4){
		he4 >> Ti >> I1i;
        Texp[i]=Ti;
        Iexp[i]=I1i;
        dTexp[i]=0.05;
        dIexp[i]=0.05;
        i++;
	}
    TGraphErrors *he4exp   = new TGraphErrors(i,Texp,Iexp,dTexp,dIexp);
    ///////////////////////////////////////////////////////	
    
//    c1.SetLogy();
    c1.SetLeftMargin(0.1);
    c1.SetRightMargin(0.05);
    c1.SetTopMargin(0.05);

	h->GetXaxis()->SetTitle("Beam energy (MeV)");
	h->GetXaxis()->CenterTitle();
	h->GetYaxis()->CenterTitle();
	h->GetYaxis()->SetTitle("Current (Amps)");
    h->GetYaxis()->SetTitleOffset(1.1);
	h->Draw();
    
    pexp->SetMarkerStyle(21);
    pexp->SetMarkerSize(0.9);
    pexp->SetMarkerColor(2);
    pexp->SetLineColor(2);
    pexp->Draw("P");
    
    dexp->SetMarkerStyle(21);
    dexp->SetMarkerSize(0.9);
    dexp->SetMarkerColor(1);
    dexp->SetLineColor(1);
    dexp->Draw("P");
    
    he3exp->SetMarkerStyle(21);
    he3exp->SetMarkerSize(0.9);
    he3exp->SetMarkerColor(3);
    he3exp->SetLineColor(3);
    he3exp->Draw("P");
    
    he4exp->SetMarkerStyle(21);
    he4exp->SetMarkerSize(0.9);
    he4exp->SetMarkerColor(4);
    he4exp->SetLineColor(4);
    he4exp->Draw("P");
    
    pteo->SetLineWidth(1.0);
    pteo->SetLineColor(2);
    pteo->SetLineStyle(1);
    pteo->Draw("L");
    
    dteo->SetLineWidth(1.0);
    dteo->SetLineColor(1);
    dteo->SetLineStyle(1);
    dteo->Draw("L");
    
    he3teo->SetLineWidth(1.0);
    he3teo->SetLineColor(3);
    he3teo->SetLineStyle(1);
    he3teo->Draw("L");
    
    he4teo->SetLineWidth(1.0);
    he4teo->SetLineColor(4);
    he4teo->SetLineStyle(1);
    he4teo->Draw("L");
    
    TLatex t;
    t.SetTextSize(0.04);
    t.DrawLatex(    30,45,"OCL");
    t.DrawLatex(    30,40,"Analysing Magnet");

    TLatex t;
    t->SetTextSize(0.04);
    TLegend *leg1 = new TLegend(0.14,0.70,0.29,0.9);
    leg1->SetBorderSize(0);
    leg1->SetFillColor(0);
    leg1->AddEntry(pexp," p","PL");
    leg1->AddEntry(dexp," d","PL");
    leg1->AddEntry(he3exp," ^{3}He","PL");
    leg1->AddEntry(he4exp," ^{4}He","PL");
    leg1->SetTextSize(0.04);
    leg1->Draw();
	
	c1->Update();
	c1->Print("anmag.pdf");
}
