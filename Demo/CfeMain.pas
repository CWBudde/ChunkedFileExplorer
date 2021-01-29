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
  protected
    procedure BaseChunkChanged;

    procedure ListViewColumns(Columns: array of string);
    procedure ListViewData(Strings: array of string);

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
var
  Index: Integer;
  Node: PVirtualNode;
  NodeData: PTreeItem;
begin
  if not Assigned(FBaseChunk) then
    exit;

  with FBaseChunk, TreeView do
  begin
    // begin update
    BeginUpdate;

    for Index := 0 to FBaseChunk.Count - 1 do
    begin
      Node := AddChild(FRootNode);
      NodeData := GetNodeData(Node);
      NodeData^.Chunk := FBaseChunk.SubChunk[Index];
      NodeData^.Name := string(FBaseChunk.SubChunk[Index].ChunkName);
    end;

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
