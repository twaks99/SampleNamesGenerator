unit DetailForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  GenerateSampleName;

type

  { TNameDetailForm }

  TNameDetailForm = class(TForm)
    btnClose: TBitBtn;
    editCity: TEdit;
    editState: TEdit;
    editZip: TEdit;
    editEmail: TEdit;
    editPhone: TEdit;
    editBirthDate: TEdit;
    editMI: TEdit;
    editName: TEdit;
    editGender: TEdit;
    editAddress: TEdit;
    lblEmail: TLabel;
    lblPhone: TLabel;
    lblBirthDate: TLabel;
    lblZip: TLabel;
    lblState: TLabel;
    lblCity: TLabel;
    lblAddress: TLabel;
    lblMI: TLabel;
    lblGender: TLabel;
    lblName: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
		SampleName: TSampleName;
  public
		procedure SetNameRecord(NameRecord: TSampleName);
  end;

var
  NameDetailForm: TNameDetailForm;

implementation

{$R *.lfm}

{ TNameDetailForm }

procedure TNameDetailForm.btnCloseClick(Sender: TObject);
begin
	self.Hide;
end;

procedure TNameDetailForm.FormActivate(Sender: TObject);
begin
  editName.SetFocus;
end;

procedure TNameDetailForm.SetNameRecord(NameRecord: TSampleName);
var
  genderText: String;
begin
  SampleName := NameRecord;
  if (SampleName.Gender = 'M') then
  	genderText := 'Male'
  else
    genderText := 'Female';

  editName.Text:= SampleName.FirstName + ' ' + SampleName.LastName;
	editMI.Text:= SampleName.MI;
  editGender.Text:= genderText;
  editAddress.Text:= SampleName.Address;
  editCity.Text:= SampleName.City;
  editState.Text:= SampleName.StateCode;
  editEmail.Text:= SampleName.Email;
  editZip.Text:= SampleName.ZipCode;
	editPhone.Text:= SampleName.PhoneNumber;
  editBirthDate.Text:= FormatDateTime('YYYY-MM-DD', SampleName.BirthDate);
  self.Caption:= 'Address Display - ID: ' + IntToStr(SampleName.ID);
end;



end.

