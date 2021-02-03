unit CfeMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, ToolWin, ActnList, StdActns, AppEvnts,
  ImgList, ImageList, Actions, Types, System.Generics.Collections,
  KControls, KHexEditor, VirtualTrees,
  CfeChunkCommon, CfeChunkPng, CfeChunkWave, CfeChunkAiff, CfeChunkMp4;

type
  TTreeItem = record
    Name: String;
    Chunk: TCustomChunk;
  end;
  PTreeItem = ^TTreeItem;

  TDisplayChunk = procedure(Chunk: TDefinedChunk) of Object;

  TFormChunkedFileExplorer = class(TForm)
    AcEditCopy: TEditCopy;
    AcEditCut: TEditCut;
    AcEditPaste: TEditPaste;
    AcEditUndo: TEditUndo;
    AcFileExit: TFileExit;
    AcFileOpen: TFileOpen;
    AcFileSaveAs: TFileSaveAs;
    ActionList: TActionList;
    CoolBar: TCoolBar;
    ListView: TListView;
    MainMenu: TMainMenu;
    MIAbout: TMenuItem;
    MiCopy: TMenuItem;
    MICut: TMenuItem;
    MIEdit: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIHelp: TMenuItem;
    MIOpen: TMenuItem;
    MIPaste: TMenuItem;
    MISave: TMenuItem;
    MISaveAs: TMenuItem;
    MIStatusBar: TMenuItem;
    MIToolbar: TMenuItem;
    MIUndo: TMenuItem;
    MIView: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    PnMain: TPanel;
    PanelDetails: TPanel;
    SpHorizontal: TSplitter;
    SpVertical: TSplitter;
    StatusBar: TStatusBar;
    TbCopy: TToolButton;
    TbCut: TToolButton;
    TbOpen: TToolButton;
    TbPaste: TToolButton;
    TbSplit1: TToolButton;
    TbSplit2: TToolButton;
    ToolBar: TToolBar;
    ToolbarImages: TImageList;
    khexeditor: TKHexEditor;
    TreeView: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AcFileOpenAccept(Sender: TObject);
    procedure MIStatusBarClick(Sender: TObject);
    procedure MIToolbarClick(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure TreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FRootNode: PVirtualNode;
    FBaseChunk: TChunkedFile;
    FDisplayProcs: TDictionary<TDefinedChunkClass, TDisplayChunk>;
    procedure InitializeDefaultListView;

    (* PNG Chunks *)
    procedure DisplayHeaderChunk(Chunk: TDefinedChunk);
    procedure DisplayPaletteChunk(Chunk: TDefinedChunk);
    procedure DisplayChromaticitiesChunk(Chunk: TDefinedChunk);
    procedure DisplayGammaChunk(Chunk: TDefinedChunk);
    procedure DisplayPhysicalDimensionsChunk(Chunk: TDefinedChunk);
    procedure DisplayPhysicalScaleChunk(Chunk: TDefinedChunk);
    procedure DisplayTextChunk(Chunk: TDefinedChunk);
    procedure DisplaySuggestedPaletteChunk(Chunk: TDefinedChunk);
    procedure DisplaySignificantBitsChunk(Chunk: TDefinedChunk);
    procedure DisplayStandardColorSpaceRGBChunk(Chunk: TDefinedChunk);
    procedure DisplayBackgroundColorChunk(Chunk: TDefinedChunk);
    procedure DisplayTransparencyChunk(Chunk: TDefinedChunk);
    procedure DisplayHistogramChunk(Chunk: TDefinedChunk);
    procedure DisplayTimeChunk(Chunk: TDefinedChunk);

    (* WAV Chunks *)
    procedure DisplayWavAFsp(Chunk: TDefinedChunk);
    procedure DisplayWavBext(Chunk: TDefinedChunk);
    procedure DisplayWavBwfAXML(Chunk: TDefinedChunk);
    procedure DisplayWavBWFLink(Chunk: TDefinedChunk);
    procedure DisplayWavCart(Chunk: TDefinedChunk);
    procedure DisplayWavFact(Chunk: TDefinedChunk);
    procedure DisplayWavInfoArtist(Chunk: TDefinedChunk);
    procedure DisplayWavInfoComment(Chunk: TDefinedChunk);
    procedure DisplayWavInfoCopyright(Chunk: TDefinedChunk);
    procedure DisplayWavInfoCreationDate(Chunk: TDefinedChunk);
    procedure DisplayWavInfoSoftwareName(Chunk: TDefinedChunk);
    procedure DisplayWavInfoSubject(Chunk: TDefinedChunk);
    procedure DisplayWavInfoTitle(Chunk: TDefinedChunk);
    procedure DisplayWavInstrument(Chunk: TDefinedChunk);
    procedure DisplayWavJunk(Chunk: TDefinedChunk);
    procedure DisplayWavLabel(Chunk: TDefinedChunk);
    procedure DisplayWavLabeledText(Chunk: TDefinedChunk);
    procedure DisplayWavNote(Chunk: TDefinedChunk);
    procedure DisplayWavPad(Chunk: TDefinedChunk);
    procedure DisplayWavSampler(Chunk: TDefinedChunk);
    procedure DisplayWavSilent(Chunk: TDefinedChunk);
    procedure DisplayWavCueChunk(Chunk: TDefinedChunk);
    procedure DisplayWavPlaylistChunk(Chunk: TDefinedChunk);

    (* AIFF Chunks *)
    procedure DisplayAiffCommon(Chunk: TDefinedChunk);
    procedure DisplayAiffForm(Chunk: TDefinedChunk);
    procedure DisplayAiffFormatVersion(Chunk: TDefinedChunk);
    procedure DisplayAiffSoundData(Chunk: TDefinedChunk);
    procedure DisplayAiffMarker(Chunk: TDefinedChunk);
    procedure DisplayAiffComment(Chunk: TDefinedChunk);
    procedure DisplayAiffInstrument(Chunk: TDefinedChunk);
    procedure DisplayAiffMidi(Chunk: TDefinedChunk);
    procedure DisplayAiffAudioRecording(Chunk: TDefinedChunk);
    procedure DisplayAiffApplicationSpecific(Chunk: TDefinedChunk);
    procedure DisplayAiffName(Chunk: TDefinedChunk);
    procedure DisplayAiffAuthor(Chunk: TDefinedChunk);
    procedure DisplayAiffCopyright(Chunk: TDefinedChunk);
    procedure DisplayAiffAnnotation(Chunk: TDefinedChunk);

    (* MP4 Chunks *)
    procedure DisplayMp4FileType(Chunk: TDefinedChunk);
    procedure DisplayMp4Moov(Chunk: TDefinedChunk);
    procedure DisplayMp4MovieHeader(Chunk: TDefinedChunk);
    procedure DisplayMp4MediaHeader(Chunk: TDefinedChunk);
    procedure DisplayMp4VideoMediaInformation(Chunk: TDefinedChunk);
    procedure DisplayMp4SoundMediaInformation(Chunk: TDefinedChunk);
    procedure DisplayMp4BaseMediaInformation(Chunk: TDefinedChunk);
    procedure DisplayMp4TrackHeader(Chunk: TDefinedChunk);
    procedure DisplayMp4SampleGroupDescription(Chunk: TDefinedChunk);
    procedure DisplayMp4SampleToGroup(Chunk: TDefinedChunk);
    procedure DisplayMp4Data(Chunk: TDefinedChunk);
    procedure DisplayMp4HandlerReference(Chunk: TDefinedChunk);
  protected
    procedure BaseChunkChanged;

    procedure ListViewColumns(Columns: array of string);
    procedure ListViewData(Strings: array of string);

    procedure DisplayFormatChunk(Chunk: TDefinedChunk);
  public
    procedure LoadFromFile(Filename: TFileName);
    procedure LoadFromStream(Stream: TStream);
  end;

var
  FormChunkedFileExplorer: TFormChunkedFileExplorer;

implementation

{$R *.dfm}

uses
  Inifiles, Math;

{ TFmPngExplorer }

procedure TFormChunkedFileExplorer.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;

  // add some spacing to our unit map treeview items
  TreeView.DefaultNodeHeight := 20;
  TreeView.NodeDataSize := SizeOf(TTreeItem);

  FDisplayProcs := TDictionary<TDefinedChunkClass, TDisplayChunk>.Create;
  FDisplayProcs.Add(TChunkPngImageHeader, DisplayHeaderChunk);
  FDisplayProcs.Add(TChunkPngPalette, DisplayPaletteChunk);
  FDisplayProcs.Add(TChunkPngGamma, DisplayGammaChunk);
  FDisplayProcs.Add(TChunkPngTime, DisplayTimeChunk);
  FDisplayProcs.Add(TChunkPngPhysicalScale, DisplayPhysicalScaleChunk);
  FDisplayProcs.Add(TCustomChunkPngText, DisplayTextChunk);
  FDisplayProcs.Add(TChunkPngStandardColorSpaceRGB, DisplayStandardColorSpaceRGBChunk);
  FDisplayProcs.Add(TChunkPngImageHistogram, DisplayHistogramChunk);
  FDisplayProcs.Add(TChunkPngBackgroundColor, DisplayBackgroundColorChunk);
  FDisplayProcs.Add(TChunkPngSuggestedPalette, DisplaySuggestedPaletteChunk);
  FDisplayProcs.Add(TChunkPngPrimaryChromaticities, DisplayChromaticitiesChunk);
  FDisplayProcs.Add(TChunkPngPhysicalPixelDimensions, DisplayPhysicalDimensionsChunk);
  FDisplayProcs.Add(TChunkPngSignificantBits, DisplaySignificantBitsChunk);
  FDisplayProcs.Add(TChunkPngTransparency, DisplayTransparencyChunk);

  FDisplayProcs.Add(TFormatChunk, DisplayFormatChunk);
  FDisplayProcs.Add(TFactChunk, DisplayWavFact);
  FDisplayProcs.Add(TInfoArtistChunk, DisplayWavInfoArtist);
  FDisplayProcs.Add(TInfoCommentChunk, DisplayWavInfoComment);
  FDisplayProcs.Add(TInfoCopyrightChunk, DisplayWavInfoCopyright);
  FDisplayProcs.Add(TInfoCreationDateChunk, DisplayWavInfoCreationDate);
  FDisplayProcs.Add(TInfoSoftwareNameChunk, DisplayWavInfoSoftwareName);
  FDisplayProcs.Add(TInfoSubjectChunk, DisplayWavInfoSubject);
  FDisplayProcs.Add(TInfoTitleChunk, DisplayWavInfoTitle);
  FDisplayProcs.Add(TInstrumentChunk, DisplayWavInstrument);
  FDisplayProcs.Add(TJunkChunk, DisplayWavJunk);
  FDisplayProcs.Add(TLabelChunk, DisplayWavLabel);
  FDisplayProcs.Add(TLabeledTextChunk, DisplayWavLabeledText);
  FDisplayProcs.Add(TNoteChunk, DisplayWavNote);
  FDisplayProcs.Add(TPadChunk, DisplayWavPad);
  FDisplayProcs.Add(TSamplerChunk, DisplayWavSampler);
  FDisplayProcs.Add(TSilentChunk, DisplayWavSilent);
  FDisplayProcs.Add(TCueChunk, DisplayWavCueChunk);
  FDisplayProcs.Add(TPlaylistChunk, DisplayWavPlaylistChunk);

  FDisplayProcs.Add(TAiffCommonChunk, DisplayAiffCommon);
  FDisplayProcs.Add(TAiffFormChunk, DisplayAiffForm);
  FDisplayProcs.Add(TAiffFormatVersionChunk, DisplayAiffFormatVersion);
  FDisplayProcs.Add(TAiffSoundDataChunk, DisplayAiffSoundData);
  FDisplayProcs.Add(TAiffMarkerChunk, DisplayAiffMarker);
  FDisplayProcs.Add(TAiffCommentChunk, DisplayAiffComment);
  FDisplayProcs.Add(TAiffInstrumentChunk, DisplayAiffInstrument);
  FDisplayProcs.Add(TAiffAudioRecordingChunk, DisplayAiffAudioRecording);
  FDisplayProcs.Add(TAiffApplicationSpecificChunk, DisplayAiffApplicationSpecific);
  FDisplayProcs.Add(TAiffNameChunk, DisplayAiffName);
  FDisplayProcs.Add(TAiffAuthorChunk, DisplayAiffAuthor);
  FDisplayProcs.Add(TAiffCopyrightChunk, DisplayAiffCopyright);
  FDisplayProcs.Add(TAiffAnnotationChunk, DisplayAiffAnnotation);

  FDisplayProcs.Add(TMp4FileTypeChunk, DisplayMp4FileType);
  FDisplayProcs.Add(TMp4MoovChunk, DisplayMp4Moov);
  FDisplayProcs.Add(TMp4MovieHeaderChunk, DisplayMp4MovieHeader);
  FDisplayProcs.Add(TMp4MediaHeaderChunk, DisplayMp4MediaHeader);
  FDisplayProcs.Add(TMp4VideoMediaInformationChunk, DisplayMp4VideoMediaInformation);
  FDisplayProcs.Add(TMp4SoundMediaInformationChunk, DisplayMp4SoundMediaInformation);
  FDisplayProcs.Add(TMp4BaseMediaInformationChunk, DisplayMp4BaseMediaInformation);
  FDisplayProcs.Add(TMp4TrackHeaderChunk, DisplayMp4TrackHeader);
  FDisplayProcs.Add(TMp4HandlerReferenceChunk, DisplayMp4HandlerReference);
  FDisplayProcs.Add(TMp4SampleGroupDescriptionChunk, DisplayMp4SampleGroupDescription);
  FDisplayProcs.Add(TMp4SampleToGroupChunk, DisplayMp4SampleToGroup);
  FDisplayProcs.Add(TMp4DataChunk, DisplayMp4Data);
end;

procedure TFormChunkedFileExplorer.FormDestroy(Sender: TObject);
begin
  // free base chunk
  FreeAndNil(FBaseChunk);
  FreeAndNil(FDisplayProcs);
end;

procedure TFormChunkedFileExplorer.FormShow(Sender: TObject);
begin
  if FileExists(ParamStr(1)) then
    LoadFromFile(ParamStr(1));
end;

procedure TFormChunkedFileExplorer.InitializeDefaultListView;
begin
  // add columns
  ListViewColumns(['Name', 'Value']);
end;

procedure TFormChunkedFileExplorer.ListViewColumns(Columns: array of string);
var
  ColumnIndex: Integer;
begin
  // clear list view
  ListView.Clear;

  // clear columns
  ListView.Columns.Clear;

  // add column
  for ColumnIndex := 0 to Length(Columns) - 1 do
    with ListView.Columns.Add do
    begin
      Caption := Columns[ColumnIndex];
      Width := Min(256, (ListView.Width - 16) div (Length(Columns)));
      MinWidth := 64;
      AutoSize := True;
    end;
end;

procedure TFormChunkedFileExplorer.ListViewData(Strings: array of string);
var
  ValueIndex: Integer;
begin
  // add data
  with ListView.Items.Add do
  begin
    Caption := Strings[0];
    for ValueIndex := 1 to Length(Strings) - 1 do
      SubItems.Add(Strings[ValueIndex]);
  end;
end;

procedure TFormChunkedFileExplorer.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else
    StatusBar.SimplePanel := False;
end;

procedure TFormChunkedFileExplorer.AcFileOpenAccept(Sender: TObject);
begin
  LoadFromFile(AcFileOpen.Dialog.Filename);
end;

procedure TFormChunkedFileExplorer.MIStatusBarClick(Sender: TObject);
begin
  MIStatusBar.Checked := not MIStatusBar.Checked;
  StatusBar.Visible := MIStatusBar.Checked;
end;

procedure TFormChunkedFileExplorer.MIToolbarClick(Sender: TObject);
begin
  MIToolbar.Checked := not MIToolbar.Checked;
  CoolBar.Visible := MIToolbar.Checked;
end;

procedure TFormChunkedFileExplorer.DisplayHeaderChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngImageHeader(Chunk) do
  begin
    ListViewData(['Width', IntToStr(Width)]);
    ListViewData(['Height', IntToStr(Height)]);
    ListViewData(['Bit Depth', IntToStr(BitDepth)]);
    ListViewData(['Color Type', ColorTypeToString(ColorType)]);
    ListViewData(['Compression Method', IntToStr(CompressionMethod)]);
    ListViewData(['Filter Method', 'Adaptive']);
    ListViewData(['Interlace Method',
      InterlaceMethodToString(InterlaceMethod)]);
    ListViewData(['HasPallette', BoolToStr(HasPalette)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayPaletteChunk(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TChunkPngPalette(Chunk) do
  begin
    ListViewColumns(['Index', 'Color']);

    for Index := 0 to Count - 1 do
      with PaletteEntry[Index] do
        ListViewData([IntToStr(Index),
          '#' + IntToHex(Integer(R shl 16 + G shl 8 + B), 6)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayGammaChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngGamma(Chunk) do
  begin
    ListViewData(['Gamma', FloatToStr(GammaAsSingle)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayHistogramChunk(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TChunkPngImageHistogram(Chunk) do
  begin
    ListViewColumns(['Index', 'Frequency']);

    for Index := 0 to Count - 1 do
      ListViewData([IntToStr(Index), IntToStr(Frequency[Index])]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplaySuggestedPaletteChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngSuggestedPalette(Chunk) do
  begin
    ListViewData(['Palette Name', string(PaletteName)]);
    ListViewData(['Palette Entries', IntToStr(Count)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplaySignificantBitsChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngSignificantBits(Chunk) do
  begin
    if SignificantBits is TPngSignificantBitsFormat0 then
      ListViewData(['Greyscale Bits',
        IntToStr(TPngSignificantBitsFormat0(SignificantBits).GrayBits)])
    else if SignificantBits is TPngSignificantBitsFormat23 then
    begin
      ListViewData(['Red Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).RedBits)]);
      ListViewData(['Green Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).GreenBits)]);
      ListViewData(['Blue Bits',
        IntToStr(TPngSignificantBitsFormat23(SignificantBits).BlueBits)]);
    end
    else if SignificantBits is TPngSignificantBitsFormat4 then
    begin
      ListViewData(['Greyscale Bits',
        IntToStr(TPngSignificantBitsFormat4(SignificantBits).GrayBits)]);
      ListViewData(['Alpha Bits',
        IntToStr(TPngSignificantBitsFormat4(SignificantBits).AlphaBits)]);
    end
    else if SignificantBits is TPngSignificantBitsFormat6 then
    begin
      ListViewData(['Red Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).RedBits)]);
      ListViewData(['Green Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).GreenBits)]);
      ListViewData(['Blue Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).BlueBits)]);
      ListViewData(['Alpha Bits',
        IntToStr(TPngSignificantBitsFormat6(SignificantBits).AlphaBits)]);
    end;
  end;
end;

procedure TFormChunkedFileExplorer.DisplayStandardColorSpaceRGBChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngStandardColorSpaceRGB(Chunk) do
  begin
    case RenderingIntent of
      0:
        ListViewData(['Rendering Indent', 'Perceptual']);
      1:
        ListViewData(['Rendering Indent', 'Relative Colorimetric']);
      2:
        ListViewData(['Rendering Indent', 'Saturation']);
      3:
        ListViewData(['Rendering Indent', 'Absolute Colorimetric']);
    else
      ListViewData(['Rendering Indent', IntToStr(RenderingIntent)]);
    end;
  end;
end;

procedure TFormChunkedFileExplorer.DisplayTextChunk(Chunk: TDefinedChunk);
begin
  with TCustomChunkPngText(Chunk) do
  begin
    ListViewData([string(Keyword), string(Text)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayTimeChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngTime(Chunk) do
  begin
    ListViewData(['Time', DateTimeToStr(ModifiedDateTime)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayTransparencyChunk(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TChunkPngTransparency(Chunk) do
  begin
    if Transparency is TPngTransparencyFormat0 then
      ListViewData(['Grey Sample Value',
        IntToStr(TPngTransparencyFormat0(Transparency).GraySampleValue)])
    else if Transparency is TPngTransparencyFormat2 then
    begin
      ListViewData(['Red Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).RedSampleValue)]);
      ListViewData(['Blue Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).BlueSampleValue)]);
      ListViewData(['Green Sample Value',
        IntToStr(TPngTransparencyFormat2(Transparency).GreenSampleValue)]);
    end
    else if Transparency is TPngTransparencyFormat3 then
      for Index := 0 to TPngTransparencyFormat3(Transparency).Count - 1 do
        ListViewData(['Index ' + IntToStr(Index),
          IntToStr(TPngTransparencyFormat3(Transparency).Transparency[Index])]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayPhysicalDimensionsChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngPhysicalPixelDimensions(Chunk) do
  begin
    ListViewData(['Pixels per unit X', IntToStr(PixelsPerUnitX)]);
    ListViewData(['Pixels per unit Y', IntToStr(PixelsPerUnitY)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayPhysicalScaleChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngPhysicalScale(Chunk) do
  begin
    ListViewData(['UnitSpecifier', IntToStr(UnitSpecifier)]);
    ListViewData(['UnitsPerPixelX', FloatToStr(UnitsPerPixelX)]);
    ListViewData(['UnitsPerPixelY', FloatToStr(UnitsPerPixelY)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayBackgroundColorChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngBackgroundColor(Chunk) do
  begin
    if Background is TPngBackgroundColorFormat04 then
      ListViewData(['Grey', IntToStr(TPngBackgroundColorFormat04(Background)
        .GraySampleValue)])
    else if Background is TPngBackgroundColorFormat26 then
    begin
      ListViewData(['Red', IntToStr(TPngBackgroundColorFormat26(Background)
        .RedSampleValue)]);
      ListViewData(['Blue', IntToStr(TPngBackgroundColorFormat26(Background)
        .BlueSampleValue)]);
      ListViewData(['Green', IntToStr(TPngBackgroundColorFormat26(Background)
        .GreenSampleValue)]);
    end
    else if Background is TPngBackgroundColorFormat3 then
      ListViewData(['Palette Index',
        IntToStr(TPngBackgroundColorFormat3(Background).PaletteIndex)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayChromaticitiesChunk(Chunk: TDefinedChunk);
begin
  with TChunkPngPrimaryChromaticities(Chunk) do
  begin
    ListViewData(['White X', FloatToStr(WhiteXAsSingle)]);
    ListViewData(['White Y', FloatToStr(WhiteYAsSingle)]);
    ListViewData(['Red X', FloatToStr(RedXAsSingle)]);
    ListViewData(['Red Y', FloatToStr(RedYAsSingle)]);
    ListViewData(['Green X', FloatToStr(GreenXAsSingle)]);
    ListViewData(['Green Y', FloatToStr(GreenYAsSingle)]);
    ListViewData(['Blue X', FloatToStr(BlueXAsSingle)]);
    ListViewData(['Blue Y', FloatToStr(BlueYAsSingle)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayFormatChunk(Chunk: TDefinedChunk);
begin
  with TFormatChunk(Chunk) do
  begin
    ListViewData(['FormatTag', IntToStr(Integer(FormatTag))]);
    ListViewData(['Channels', IntToStr(Channels)]);
    ListViewData(['SampleRate', IntToStr(SampleRate)]);
    ListViewData(['BytesPerSecond', IntToStr(BytesPerSecond)]);
    ListViewData(['BlockAlign', IntToStr(BlockAlign)]);
    ListViewData(['BitsPerSample', IntToStr(BitsPerSample)]);
    ListViewData(['ValidBitsPerSample', IntToStr(ValidBitsPerSample)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavFact(Chunk: TDefinedChunk);
begin
  with TFactChunk(Chunk) do
  begin
    ListViewData(['SampleCount', IntToStr(SampleCount)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoSoftwareName(Chunk: TDefinedChunk);
begin
  with TInfoSoftwareNameChunk(Chunk) do
  begin
    ListViewData(['SoftwareName', SoftwareName]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoComment(Chunk: TDefinedChunk);
begin
  with TInfoCommentChunk(Chunk) do
  begin
    ListViewData(['Comment', Comment]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoCreationDate(Chunk: TDefinedChunk);
begin
  with TInfoCreationDateChunk(Chunk) do
  begin
    ListViewData(['CreationDate', CreationDate]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoCopyright(Chunk: TDefinedChunk);
begin
  with TInfoCopyrightChunk(Chunk) do
  begin
    ListViewData(['Copyright', Copyright]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoSubject(Chunk: TDefinedChunk);
begin
  with TInfoSubjectChunk(Chunk) do
  begin
    ListViewData(['Subject', Subject]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoArtist(Chunk: TDefinedChunk);
begin
  with TInfoArtistChunk(Chunk) do
  begin
    ListViewData(['Artist', Artist]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInfoTitle(Chunk: TDefinedChunk);
begin
  with TInfoTitleChunk(Chunk) do
  begin
    ListViewData(['Title', Title]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavLabel(Chunk: TDefinedChunk);
begin
  with TLabelChunk(Chunk) do
  begin
    ListViewData(['Label', Text]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavNote(Chunk: TDefinedChunk);
begin
  with TNoteChunk(Chunk) do
  begin
    ListViewData(['Note', Note]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavLabeledText(Chunk: TDefinedChunk);
begin
  with TLabeledTextChunk(Chunk) do
  begin
    ListViewData(['Text', Text]);
    ListViewData(['CuePoint ID', IntToStr(CuePointID)]);
    ListViewData(['SampleLength', IntToStr(SampleLength)]);
    ListViewData(['Purpose ID', IntToStr(PurposeID)]);
    ListViewData(['Country', IntToStr(Country)]);
    ListViewData(['Language', IntToStr(Language)]);
    ListViewData(['Dialect', IntToStr(Dialect)]);
    ListViewData(['Code Page', IntToStr(CodePage)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavPlaylistChunk(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TPlaylistChunk(Chunk) do
  begin
    ListViewColumns(['CuePointID', 'LengthInSamples', 'NumberOfRepeats']);

    for Index := 0 to PlaylistSegments.Count - 1 do
      with TPlaylistSegmentItem(PlaylistSegments.Items[Index]) do
        ListViewData([
          IntToStr(CuePointID),
          IntToStr(LengthInSamples),
          IntToStr(NumberOfRepeats)
        ]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavSilent(Chunk: TDefinedChunk);
begin
  with TSilentChunk(Chunk) do
  begin
    ListViewData(['Number of Silent Samples', IntToStr(NumberOfSilentSamples)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavJunk(Chunk: TDefinedChunk);
begin
  with TJunkChunk(Chunk) do
  begin
    ListViewData(['Padding', IntToStr(Padding)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavPad(Chunk: TDefinedChunk);
begin
  with TPadChunk(Chunk) do
  begin
    ListViewData(['Align Size', IntToStr(AlignSize)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavCueChunk(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TCueChunk(Chunk) do
  begin
    ListViewColumns(['CuePointName', 'CuePointSamplePosition',
      'FileStartPosition', 'RelativeBlockStartPosition',
      'RelativeBlockSampleOffset']);

    for Index := 0 to CueCollection.Count - 1 do
      ListViewData([
        TCueItem(CueCollection.Items[Index]).CuePointName,
        IntToStr(TCueItem(CueCollection.Items[Index]).CuePointSamplePosition),
        IntToStr(TCueItem(CueCollection.Items[Index]).FileStartPosition),
        IntToStr(TCueItem(CueCollection.Items[Index]).RelativeBlockStartPosition),
        IntToStr(TCueItem(CueCollection.Items[Index]).RelativeBlockSampleOffset)
      ]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavSampler(Chunk: TDefinedChunk);
begin
  with TSamplerChunk(Chunk) do
  begin
    ListViewData(['Manufacturer', IntToStr(Integer(Manufacturer))]);
    ListViewData(['Product', IntToStr(Product)]);
    ListViewData(['Sample Period', IntToStr(SamplePeriod)]);
    ListViewData(['MIDI Unity Note', IntToStr(MIDIUnityNote)]);
    ListViewData(['MIDI Pitch Fraction', IntToStr(MIDIPitchFraction)]);
    ListViewData(['SMPTE Format', IntToStr(Integer(SMPTEFormat))]);
    ListViewData(['SMPTE Offset', IntToStr(SMPTEOffset)]);
    ListViewData(['NumSample Loops', IntToStr(NumSampleLoops)]);
    ListViewData(['Sampler Data', IntToStr(SamplerData)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavInstrument(Chunk: TDefinedChunk);
begin
  with TInstrumentChunk(Chunk) do
  begin
    ListViewData(['Unshifted Note', IntToStr(UnshiftedNote)]);
    ListViewData(['Fine Tune', IntToStr(FineTune)]);
    ListViewData(['Gain dB', IntToStr(Gain_dB)]);
    ListViewData(['Low Note', IntToStr(LowNote)]);
    ListViewData(['High Note', IntToStr(HighNote)]);
    ListViewData(['Low Velocity', IntToStr(LowVelocity)]);
    ListViewData(['High Velocity', IntToStr(HighVelocity)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavBext(Chunk: TDefinedChunk);
begin
  with TBextChunk(Chunk) do
  begin
    ListViewData(['Description', Description]);
    ListViewData(['Originator', Originator]);
    ListViewData(['Originator Ref', OriginatorRef]);
    ListViewData(['Origination Date', OriginationDate]);
    ListViewData(['Origination Time', OriginationTime]);
    ListViewData(['Time Ref Low', IntToStr(TimeRefLow)]);
    ListViewData(['Time Ref High', IntToStr(TimeRefHigh)]);
    ListViewData(['Version', IntToStr(Version)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavCart(Chunk: TDefinedChunk);
begin
  with TCartChunk(Chunk) do
  begin
    ListViewData(['Title', Title]);
    ListViewData(['Artist', Artist]);
    ListViewData(['Cut ID', CutID]);
    ListViewData(['Client ID', ClientID]);
    ListViewData(['Category', Category]);
    ListViewData(['Classification', Classification]);
    ListViewData(['Out Cue', OutCue]);
    ListViewData(['Start Date', StartDate]);
    ListViewData(['Start Time', StartTime]);
    ListViewData(['End Date', EndDate]);
    ListViewData(['End Time', EndTime]);
    ListViewData(['Producer App ID', ProducerAppID]);
    ListViewData(['Producer App Version', ProducerAppVersion]);
    ListViewData(['User Def', UserDef]);
    ListViewData(['dB Level Reference', IntToStr(dbLevelReference)]);
    ListViewData(['Version', IntToStr(Version)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavAFsp(Chunk: TDefinedChunk);
begin
  with TWavAFspChunk(Chunk) do
  begin
    ListViewData(['Text', Text]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavBWFLink(Chunk: TDefinedChunk);
begin
  with TBWFLinkChunk(Chunk) do
  begin
    ListViewData(['XMLData', XMLData]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayWavBwfAXML(Chunk: TDefinedChunk);
begin
  with TBwfAXMLChunk(Chunk) do
  begin
    ListViewData(['XMLData', XMLData]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffCommon(Chunk: TDefinedChunk);
begin
  with TAiffCommonChunk(Chunk) do
  begin
    ListViewData(['Channels', IntToStr(Channels)]);
    ListViewData(['Sampleframes', IntToStr(SampleFrames)]);
    ListViewData(['Samplesize', IntToStr(SampleSize)]);
    ListViewData(['Samplerate', FloatToStr(SampleRate)]);
    ListViewData(['Compression', IntToStr(Integer(Compression))]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffForm(Chunk: TDefinedChunk);
begin
  with TAiffFormChunk(Chunk) do
  begin
    ListViewData(['FormType', IntToStr(Integer(FormType))]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffFormatVersion(Chunk: TDefinedChunk);
begin
  with TAiffFormatVersionChunk(Chunk) do
  begin
    ListViewData(['FormatVersion', IntToStr(TimeStamp)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffSoundData(Chunk: TDefinedChunk);
begin
  with TAiffSoundDataChunk(Chunk) do
  begin
    ListViewData(['Offset', IntToStr(Offset)]);
    ListViewData(['BlockSize', IntToStr(BlockSize)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffMarker(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TAiffMarkerChunk(Chunk) do
  begin
    ListViewColumns(['Marker Name', 'Marker ID', 'Position']);
    for Index := 0 to MarkerCount - 1 do
      ListViewData([Marker[Index].MarkerName, IntToStr(Marker[Index].MarkerID), IntToStr(Marker[Index].Position)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffComment(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TAiffCommentChunk(Chunk) do
  begin
    ListViewColumns(['Comment', 'Marker ID', 'TimeStamp']);
    for Index := 0 to CommentCount - 1 do
      ListViewData([Comment[Index].Comment, IntToStr(Comment[Index].MarkerID), IntToStr(Comment[Index].TimeStamp)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffInstrument(Chunk: TDefinedChunk);
begin
  with TAiffInstrumentChunk(Chunk) do
  begin
    ListViewData(['BaseNote', IntToStr(BaseNote)]);
    ListViewData(['Detune', IntToStr(Detune)]);
    ListViewData(['Low Note', IntToStr(LowNote)]);
    ListViewData(['High Note', IntToStr(HighNote)]);
    ListViewData(['Low Velocity', IntToStr(LowVelocity)]);
    ListViewData(['High Velocity', IntToStr(HighVelocity)]);
    ListViewData(['Gain', IntToStr(Gain)]);
    ListViewData(['Sustain Loop', IntToStr(SustainLoop)]);
    ListViewData(['Release Loop', IntToStr(ReleaseLoop)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffMidi(Chunk: TDefinedChunk);
begin
  with TAiffMIDIChunk(Chunk) do
  begin
//    ListViewData(['MIDIData', IntToStr(MIDIData)]);
  end;
end;

(*
  TAiffMIDIChunk = class(TAiffDefinedChunk)
    property MIDIData[index: Integer]: Byte read GetMIDIData;
*)

procedure TFormChunkedFileExplorer.DisplayAiffAudioRecording(Chunk: TDefinedChunk);
begin
  with TAiffAudioRecordingChunk(Chunk) do
  begin
    ListViewData(['AES Channel Status Data', AESChannelStatusData]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffApplicationSpecific(Chunk: TDefinedChunk);
begin
  with TAiffApplicationSpecificChunk(Chunk) do
  begin
    ListViewData(['Application Signature', ApplicationSignature]);
    ListViewData(['Application Data', ApplicationDataAsString]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffName(Chunk: TDefinedChunk);
begin
  with TAiffNameChunk(Chunk) do
  begin
    ListViewData(['Name', Name]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffAuthor(Chunk: TDefinedChunk);
begin
  with TAiffAuthorChunk(Chunk) do
  begin
    ListViewData(['Author', Author]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffCopyright(Chunk: TDefinedChunk);
begin
  with TAiffCopyrightChunk(Chunk) do
  begin
    ListViewData(['Copyright', Copyright]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayAiffAnnotation(Chunk: TDefinedChunk);
begin
  with TAiffAnnotationChunk(Chunk) do
  begin
    ListViewData(['Annotation', Annotation]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4FileType(Chunk: TDefinedChunk);
begin
  with TMp4FileTypeChunk(Chunk) do
  begin
    ListViewData(['Major Brand', MajorBrand]);
    ListViewData(['Minor Version', IntToStr(MinorVersion)]);
    ListViewData(['Compatible Brands', CompatibleBrandsAsString]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4Moov(Chunk: TDefinedChunk);
begin
  with TMp4MoovChunk(Chunk) do
  begin
    ListViewData(['Sub Chunk Count', IntToStr(Count)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4MovieHeader(Chunk: TDefinedChunk);
begin
  with TMp4MovieHeaderChunk(Chunk) do
  begin
    ListViewData(['CreationTime', DateTimeToStr(CreationTime)]);
    ListViewData(['ModificationTime', DateTimeToStr(ModificationTime)]);
    ListViewData(['TimeScale', FloatToStr(TimeScale) + ' units / s']);
    ListViewData(['Duration', FloatToStr(Duration) + ' s']);
    ListViewData(['PreferredRate', FloatToStr(PreferredRate)]);
    ListViewData(['PreferredVolume', FloatToStr(PreferredVolume)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4TrackHeader(Chunk: TDefinedChunk);
begin
  with TMp4TrackHeaderChunk(Chunk) do
  begin
    ListViewData(['CreationTime', DateTimeToStr(CreationTime)]);
    ListViewData(['ModificationTime', DateTimeToStr(ModificationTime)]);
  end;
end;

(*
  TMp4EditListEntry = class
  private
    FTrackDuration: Cardinal;
    FMediaTime: Cardinal;
    FMediaRate: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4EditListEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4EditListEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4EditListEntry);
  public
    function Add(AChunk: TMp4EditListEntry): Integer;
    function Extract(Item: TMp4EditListEntry): TMp4EditListEntry;
    function Remove(AChunk: TMp4EditListEntry): Integer;
    function IndexOf(AChunk: TMp4EditListEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4EditListEntry);
    function First: TMp4EditListEntry;
    function Last: TMp4EditListEntry;

    property Items[Index: Integer]: TMp4EditListEntry read GetItem write SetItem; default;
  end;

  TMp4EditListChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FList: TMp4EditListEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4EditChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;
*)

procedure TFormChunkedFileExplorer.DisplayMp4MediaHeader(Chunk: TDefinedChunk);
begin
  with TMp4MediaHeaderChunk(Chunk) do
  begin
    ListViewData(['CreationTime', DateTimeToStr(CreationTime)]);
    ListViewData(['ModificationTime', DateTimeToStr(ModificationTime)]);
    ListViewData(['TimeScale', FloatToStr(TimeScale) + ' Hz']);
    ListViewData(['Duration', FloatToStr(Duration) + ' s']);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4HandlerReference(Chunk: TDefinedChunk);
begin
  with TMp4HandlerReferenceChunk(Chunk) do
  begin
    ListViewData(['Component Type', ComponentType]);
    ListViewData(['Component Subtype', ComponentSubType]);
    ListViewData(['Component Manufacturer', ComponentManufacturer]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4VideoMediaInformation(Chunk: TDefinedChunk);
begin
  with TMp4VideoMediaInformationChunk(Chunk) do
  begin
    ListViewData(['GraphicsMode', IntToStr(GraphicsMode)]);
(*
    ListViewData(['OpColor[0]', OpColor[0]]);
    ListViewData(['OpColor[1]', OpColor[1]]);
    ListViewData(['OpColor[2]', OpColor[2]]);
*)
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4SoundMediaInformation(Chunk: TDefinedChunk);
begin
  with TMp4SoundMediaInformationChunk(Chunk) do
  begin
    ListViewData(['Balance', IntToStr(Balance)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4BaseMediaInformation(Chunk: TDefinedChunk);
begin
  with TMp4BaseMediaInformationChunk(Chunk) do
  begin
    ListViewData(['GraphicsMode', IntToStr(GraphicsMode)]);
    ListViewData(['Balance', IntToStr(Balance)]);
  end;
end;

(*
  TMp4DataReferenceChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4DataReference(Chunk: TDefinedChunk);
begin
  with TMp4DataReferenceChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4ElementaryStreamDescriptorChunk = class(TMp4DefinedChunk)
  private
    FBitStream: TMemoryStream;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4ElementaryStreamDescriptor(Chunk: TDefinedChunk);
begin
  with TMp4ElementaryStreamDescriptorChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4Mp4aChunk = class(TMp4ContainerChunk)
  private
    FUnknownA: array [0..11] of Byte;
    FDataReferenceIndex: Cardinal;
    FUnknownB: array [0..7] of Byte;
    FChannelCount: Cardinal;
    FSampleSize: Cardinal;
    FUnknownC: array [0..1] of Byte;
    FSampleRate: Cardinal;
    FUnknownD: array [0..1] of Byte;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4SampleDescriptionChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4SampleDescription(Chunk: TDefinedChunk);
begin
  with TMp4SampleDescriptionChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4SampleTableEntry = class
  private
    FSampleCount: Cardinal;
    FSampleDuration: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4SampleTableEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleTableEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleTableEntry);
  public
    function Add(AChunk: TMp4SampleTableEntry): Integer;
    function Extract(Item: TMp4SampleTableEntry): TMp4SampleTableEntry;
    function Remove(AChunk: TMp4SampleTableEntry): Integer;
    function IndexOf(AChunk: TMp4SampleTableEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleTableEntry);
    function First: TMp4SampleTableEntry;
    function Last: TMp4SampleTableEntry;

    property Items[Index: Integer]: TMp4SampleTableEntry read GetItem write SetItem; default;
  end;

  TMp4TimeToSampleChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleTable: TMp4SampleTableEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4SampleToChunkEntry = class
  private
    FFirstChunk: Cardinal;
    FSamplesPerChunk: Cardinal;
    FSampleDescription: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

  TMp4SampleToChunkEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleToChunkEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleToChunkEntry);
  public
    function Add(AChunk: TMp4SampleToChunkEntry): Integer;
    function Extract(Item: TMp4SampleToChunkEntry): TMp4SampleToChunkEntry;
    function Remove(AChunk: TMp4SampleToChunkEntry): Integer;
    function IndexOf(AChunk: TMp4SampleToChunkEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleToChunkEntry);
    function First: TMp4SampleToChunkEntry;
    function Last: TMp4SampleToChunkEntry;

    property Items[Index: Integer]: TMp4SampleToChunkEntry read GetItem write SetItem; default;
  end;

  TMp4SampleToChunkChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleToChunkTable: TMp4SampleToChunkEntryList;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4SampleSizeChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FSampleSize: Cardinal;
    FNumberOfEntries: Cardinal;
    FSampleSizes: array of Cardinal;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
  end;
end;
*)

(*
  TMp4ChunkOffsetChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FChunkOffsetTable: array of Int64;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
  end;
end;
*)

procedure TFormChunkedFileExplorer.DisplayMp4SampleGroupDescription(Chunk: TDefinedChunk);
begin
  with TMp4SampleGroupDescriptionChunk(Chunk) do
  begin
    ListViewData(['DefaultLength', IntToStr(DefaultLength)]);
    ListViewData(['EntryCount', IntToStr(EntryCount)]);
  end;
end;

procedure TFormChunkedFileExplorer.DisplayMp4SampleToGroup(Chunk: TDefinedChunk);
var
  Index: Integer;
begin
  with TMp4SampleToGroupChunk(Chunk) do
  begin
    ListViewColumns(['SampleCount', 'GroupDescriptionIndex']);

    for Index := 0 to TableData.Count - 1 do
      ListViewData([IntToStr(TableData[Index].SampleCount), IntToStr(TableData[Index].GroupDescriptionIndex)]);
  end;
end;


procedure TFormChunkedFileExplorer.DisplayMp4Data(Chunk: TDefinedChunk);
begin
  with TMp4DataChunk(Chunk) do
  begin
    ListViewData(['Type', IntToStr(&Type)]);
    ListViewData(['Locale', IntToStr(Locale)]);
    ListViewData(['Data', DataAsString]);
  end;
end;

procedure TFormChunkedFileExplorer.TreeViewChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PTreeItem;
  MemStream: TMemoryStream;
begin
  if Node = nil then
    exit;

  NodeData := Sender.GetNodeData(Node);

  // display chunk size
  if TObject(NodeData.Chunk) is TDefinedChunk then
    StatusBar.SimpleText := 'Chunk Size: ' +
      IntToStr(TDefinedChunk(NodeData.Chunk).ChunkSize);

  if FDisplayProcs.ContainsKey(TDefinedChunkClass(NodeData.Chunk.ClassType)) then
  begin
    InitializeDefaultListView;
    FDisplayProcs[TDefinedChunkClass(NodeData.Chunk.ClassType)](TDefinedChunk(NodeData.Chunk));
    ListView.BringToFront;
  end
  else
    // other unregistered chunks
    if TObject(NodeData.Chunk) is TCustomChunk then
      with TCustomChunk(NodeData.Chunk) do
      begin
        InitializeDefaultListView;

        ListViewData(['Chunk Name', string(ChunkNameAsString)]);
        ListViewData(['Chunk Size', IntToStr(ChunkSize)]);

        ListView.BringToFront;
      end;

  with NodeData.Chunk do
  begin
    MemStream := TMemoryStream.Create;
    SaveToStream(MemStream);
    MemStream.Position := 0;
    KHexEditor.LoadFromStream(MemStream);
    KHexEditor.Invalidate;
    MemStream.Free;
  end;
end;

procedure TFormChunkedFileExplorer.TreeViewFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PTreeItem;
begin
  NodeData := Sender.GetNodeData(Node);
  NodeData^.Chunk := nil;
  Finalize(NodeData^);
end;

procedure TFormChunkedFileExplorer.TreeViewGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PTreeItem;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData.Name;
end;

procedure TFormChunkedFileExplorer.BaseChunkChanged;

  procedure ProcessChunk(Chunk: TCustomChunkContainer; Root: PVirtualNode);
  var
    Index: Integer;
    Node: PVirtualNode;
    NodeData: PTreeItem;
  begin
    for Index := 0 to Chunk.Count - 1 do
    begin
      Node := TreeView.AddChild(Root);
      NodeData := TreeView.GetNodeData(Node);
      NodeData^.Chunk := Chunk.SubChunk[Index];
      NodeData^.Name := string(Chunk.SubChunk[Index].ChunkName);

       // eventually crawl sub container
      if Chunk.SubChunk[Index] is TCustomChunkContainer then
        ProcessChunk(TCustomChunkContainer(Chunk.SubChunk[Index]), Node);
    end;
  end;

begin
  if not Assigned(FBaseChunk) then
    exit;

  with FBaseChunk, TreeView do
  begin
    // begin update
    BeginUpdate;

    ProcessChunk(FBaseChunk, FRootNode);

    // expand tree
    FullExpand(FRootNode);

    // end update
    EndUpdate;
  end;
end;

procedure TFormChunkedFileExplorer.LoadFromFile(Filename: TFileName);
var
  Start, Stop, Freq: Int64;
  MemoryFileStream: TMemoryStream;
  NodeData: PTreeItem;
begin
  if not FileExists(Filename) then
    Exit;

  // initialize listview
  InitializeDefaultListView;

  // create temporary memory strem
  MemoryFileStream := TMemoryStream.Create;
  try
    // query start
    QueryPerformanceCounter(Start);

    // load data from file
    MemoryFileStream.LoadFromFile(Filename);

    // query stop
    QueryPerformanceCounter(Stop);

    // query performance frequency
    QueryPerformanceFrequency(Freq);

    // add loading TimeChunk
    ListViewData(['loading time', FloatToStrF((Stop - Start) * 1000 / Freq,
      ffGeneral, 4, 4) + ' ms']);

    LoadFromStream(MemoryFileStream);

    NodeData := TreeView.GetNodeData(FRootNode);
    NodeData.Name := Filename;
  finally
    FreeAndNil(MemoryFileStream);
  end;
end;

procedure TFormChunkedFileExplorer.LoadFromStream(Stream: TStream);
var
  Start, Stop, Freq: Int64;
  ChunkName: TChunkName;
  ChunkSize: Cardinal absolute ChunkName;
  NodeData: PTreeItem;
begin
  // reset stream position
  Stream.Position := 0;

  // query start
  QueryPerformanceCounter(Start);

  if TFilePng.CanLoad(Stream) then
  begin
    FBaseChunk := TFilePng.Create;
    FBaseChunk.LoadFromStream(Stream);
  end
  else
  if TFileWave.CanLoad(Stream) then
  begin
    FBaseChunk := TFileWave.Create;
    FBaseChunk.LoadFromStream(Stream);
  end
  else
  if TFileAiff.CanLoad(Stream) then
  begin
    FBaseChunk := TFileAiff.Create;
    FBaseChunk.LoadFromStream(Stream);
  end
  else
  if TFileMp4.CanLoad(Stream) then
  begin
    FBaseChunk := TFileMp4.Create;
    FBaseChunk.LoadFromStream(Stream);
  end;

  // query stop
  QueryPerformanceCounter(Stop);

  // query performance frequency
  QueryPerformanceFrequency(Freq);

  // initialize listview
  InitializeDefaultListView;

  // add loading TimeChunk
  ListViewData(['loading time', FloatToStrF((Stop - Start) * 1000 / Freq,
    ffGeneral, 4, 4) + ' ms']);

  // clear existing items on treeview
  TreeView.Clear;

  // create root node for project
  FRootNode := TreeView.AddChild(TreeView.RootNode);
  NodeData := TreeView.GetNodeData(FRootNode);
  NodeData.Chunk := FBaseChunk;

  // query start
  QueryPerformanceCounter(Start);

  BaseChunkChanged;

  // query stop
  QueryPerformanceCounter(Stop);

  // add building tree TimeChunk
  ListViewData(['building tree time', FloatToStrF((Stop - Start) * 1000 / Freq,
    ffGeneral, 4, 4) + ' ms']);

  ListView.BringToFront;

  // change caption
  Caption := 'Chunked File Explorer';
end;

end.
