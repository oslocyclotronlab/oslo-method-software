// -*- c++ -*-

#ifndef RUPDATE_H
#define RUPDATE_H 1

#include <TGFrame.h>
#include <vector>

class TGCheckButton;
class TGComboBox;
class TGLabel;
class TGNumberEntry;
class TGRadioButton;
class TGStatusBar;
class TGTextButton;
class TGTextEntry;
class TH1;
class TH2;
class TRootEmbeddedCanvas;

class MyMainFrame : public TGMainFrame {
public:
    MyMainFrame();
    virtual ~MyMainFrame();

    void CloseWindow();

    void OnComboBeamSelect(Int_t);
    void OnDetectorsForward();
    void OnDetectorsBackward();
    void OnTarget12C();
    void OnTargetAny();
    void OnTargetZ(Long_t);
    void OnTargetZText(const char*);
    void OnCanvasEvent(Int_t event, Int_t x, Int_t y, TObject* obj);
    void OnButtonCalc();

private:
    void LoadDefaults();
    void SaveDefaults();

    ClassDef(MyMainFrame,0)

private:
    TGComboBox    *cmb_beam;
    TGNumberEntry *n_energy, *n_targetA, *n_targetZ, *n_targetT, *n_targetT_C12, *n_det;
    TGRadioButton *r_c12, *r_anytarget, *r_forward, *r_backward;
    TRootEmbeddedCanvas *ecanvas;
    TGStatusBar   *statusbar;
    TGTextEntry   *t_targetName;
};

#endif /* RUPDATE_H */

