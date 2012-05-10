!include "MUI.nsh"

SetCompressor /FINAL lzma
Name "Daisy ${VERSION}"
OutFile "daisy-${VERSION}-setup.exe"
InstallDir "$PROGRAMFILES\Daisy ${VERSION}"
InstallDirRegKey HKCU "Software\Daisy ${VERSION}" "Install Directory"

!define MUI_ABORTWARNING

!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
  
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
  
!insertmacro MUI_LANGUAGE "English"

Section "Main Install"
  SetOutPath $INSTDIR
  File /r "install\*.*"
  WriteRegStr HKCU "Software\Daisy ${VERSION}" "Install Directory" $INSTDIR
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  CreateDirectory "$SMPROGRAMS\Daisy ${VERSION}"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Home Page.lnk" "http://www.dina.kvl.dk/~daisy/"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Explore.lnk" "$INSTDIR"
  CreateShortCut "$SMPROGRAMS\Daisy ${VERSION}\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\Uninstall.exe"
  RMDir /r "$INSTDIR"
  DeleteRegValue HKCU "Software\Daisy ${VERSION}" "Install Directory"
  DeleteRegKey /ifempty HKCU "Software\Daisy ${VERSION}"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Daisy.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Home Page.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Explore.lnk"
  Delete "$SMPROGRAMS\Daisy ${VERSION}\Uninstall.lnk"
  RMDir "$SMPROGRAMS\Daisy ${VERSION}"
SectionEnd
