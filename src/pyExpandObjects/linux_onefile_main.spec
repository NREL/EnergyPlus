# -*- mode: python ; coding: utf-8 -*-

import os
block_cipher = None

spec_root = os.path.abspath(SPECPATH)

a = Analysis(['src/main.py'],
             pathex=[os.path.join(spec_root, 'src')],
             binaries=[],
             datas=[
                  ('src/resources/Energy+.schema.epJSON', 'resources'),
                  ('src/resources/template_expansion_structures.yaml', 'resources'),
                  ('logs/logging.conf', 'logs')],
             hiddenimports=['custom_exceptions', 'epjson_handler', 'expand_objects', 'hvac_template', 'logger'],
             hookspath=[],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher,
             noarchive=False)
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='pyExpandObjects',
          debug=False,
          strip=False,
          upx=True,
          console=True )
