!include "MUI.nsh"

SetCompressor /FINAL lzma
Name "Daisy ${VERSION}"
OutFile "daisy-${VERSION}-setup-w64.exe"
InstallDir "$PROGRAMFILES64\Daisy ${VERSION}"
InstallDirRegKey HKCU "Software\Daisy ${VERSION} (w64)" "Install Directory"

!define MUI_ABORTWARNING

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
  
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
  
!insertmacro MUI_LANGUAGE "English"

Section "Main Install"
  SetOutPath $INSTDIR
  File /r "install\*.*"
  WriteRegStr HKCU "Software\Daisy ${VERSION} (w64)" "Install Directory" $INSTDIR
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  CreateDirectory "$SMPROGRAMS\Daisy ${VERSION}"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\ShowDaisyOutput.lnk" "$INSTDIR\bin\ShowDaisyOutput.exe"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Home Page.lnk" "http://daisy.ku.dk/"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Explore.lnk" "$INSTDIR"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r "$INSTDIR"
  DeleteRegValue HKCU "Software\Daisy ${VERSION} (w64)" "Install Directory"
  DeleteRegKey /ifempty HKCU "Software\Daisy ${VERSION} (w64)"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\ShowDaisyOutput.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Home Page.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Explore.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Uninstall.lnk"
  RMDir "$SMPROGRAMS\Daisy ${VERSION}"
SectionEnd
