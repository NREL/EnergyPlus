/************
*
*   This file is part of a tool for producing 3D content in the PRC format.
*   Copyright (C) 2008  Orest Shardt <shardtor (at) gmail dot com>
*
*   This program is free software: you can redistribute it and/or modify
*   it under the terms of the GNU Lesser General Public License as published by
*   the Free Software Foundation, either version 3 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU Lesser General Public License for more details.
*
*   You should have received a copy of the GNU Lesser General Public License
*   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
*************/

#ifndef __O_PRC_FILE_H
#define __O_PRC_FILE_H

#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <set>
#include <list>
#include <stack>
#include <string>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "PRC.h"
#include "PRCbitStream.h"
#include "writePRC.h"

class oPRCFile;
class PRCFileStructure;

#define EQFLD(fld) fld==c.fld
#define COMPFLD(fld) \
if(fld != c.fld) \
return (fld < c.fld);

struct RGBAColour
{
  RGBAColour(double r=0.0, double g=0.0, double b=0.0, double a=1.0) :
    R(r), G(g), B(b), A(a) {}
  double R,G,B,A;

  void Set(double r, double g, double b, double a=1.0) 
  {
    R = r; G = g; B = b; A = a;
  }
  bool operator==(const RGBAColour &c) const
  {
    return (EQFLD(R) && EQFLD(G) && EQFLD(B) && EQFLD(A));
  }
  bool operator!=(const RGBAColour &c) const
  {
    return !(EQFLD(R) && EQFLD(G) && EQFLD(B) && EQFLD(A));
  }
  bool operator<(const RGBAColour &c) const
  {
    COMPFLD(R)
    COMPFLD(G)
    COMPFLD(B)
    COMPFLD(A)
    return false;
  }
  friend RGBAColour operator * (const RGBAColour& a, const double d)
  { return RGBAColour(a.R*d,a.G*d,a.B*d,a.A*d); }
  friend RGBAColour operator * (const double d, const RGBAColour& a)
  { return RGBAColour(a.R*d,a.G*d,a.B*d,a.A*d); }
};
typedef std::map<RGBAColour,uint32_t> PRCcolourMap;

struct RGBAColourWidth
{
  RGBAColourWidth(double r=0.0, double g=0.0, double b=0.0, double a=1.0, double w=1.0) :
    R(r), G(g), B(b), A(a), W(w) {}
  double R,G,B,A,W;

  bool operator==(const RGBAColourWidth &c) const
  {
    return (EQFLD(R) && EQFLD(G) && EQFLD(B) && EQFLD(A) && EQFLD(W));
  }
  bool operator!=(const RGBAColourWidth &c) const
  {
    return !(EQFLD(R) && EQFLD(G) && EQFLD(B) && EQFLD(A) && EQFLD(W));
  }
  bool operator<(const RGBAColourWidth &c) const
  {
    COMPFLD(R)
    COMPFLD(G)
    COMPFLD(B)
    COMPFLD(A)
    COMPFLD(W)
    return false;
  }
};
typedef std::map<RGBAColourWidth,uint32_t> PRCcolourwidthMap;

struct PRCmaterial
{
  PRCmaterial() : alpha(1.0),shininess(1.0),width(1.0) {}
  PRCmaterial(const RGBAColour &a, const RGBAColour &d, const RGBAColour &e,
              const RGBAColour &s, double p, double h, double w=1.0) :
  ambient(a), diffuse(d), emissive(e), specular(s), alpha(p), shininess(h), width(w) {}
  
  RGBAColour ambient,diffuse,emissive,specular;
  double alpha,shininess,width;
  
  bool operator==(const PRCmaterial &c) const
  {
    return (EQFLD(ambient)   &&
            EQFLD(diffuse)   &&
            EQFLD(emissive)  &&
            EQFLD(specular)  &&
            EQFLD(alpha)     &&
            EQFLD(shininess) &&
            EQFLD(width));
  }
  bool operator<(const PRCmaterial &c) const
  {
    COMPFLD(ambient)
    COMPFLD(diffuse)
    COMPFLD(emissive)
    COMPFLD(specular)
    COMPFLD(alpha)
    COMPFLD(shininess)
    COMPFLD(width)
    return false;
  }
};
typedef std::map<PRCmaterial,uint32_t> PRCmaterialMap;

#undef EQFLD
#undef COMPFLD
                 
struct PRCtexture
{
  PRCtexture() : 
  data(NULL), format(KEPRCPicture_BITMAP_RGBA_BYTE), width(0), height(0), size(0),
  mapping(0), components(PRC_TEXTURE_MAPPING_COMPONENTS_RGBA),
  function(KEPRCTextureFunction_Modulate),
  wrapping_mode_S(KEPRCTextureWrappingMode_Repeat), wrapping_mode_T(KEPRCTextureWrappingMode_Repeat) {}
  
  const uint8_t* data;
  EPRCPictureDataFormat format;
  /*
    KEPRCPicture_PNG
    KEPRCPicture_JPG
    KEPRCPicture_BITMAP_RGB_BYTE
    KEPRCPicture_BITMAP_RGBA_BYTE
    KEPRCPicture_BITMAP_GREY_BYTE
    KEPRCPicture_BITMAP_GREYA_BYTE
  */
  uint32_t width;   // may be omitted for PNG and JPEG
  uint32_t height;  // too
  uint32_t size;
  
  uint32_t mapping;
  /*
  PRC_TEXTURE_MAPPING_DIFFUSE
  PRC_TEXTURE_MAPPING_BUMP
  PRC_TEXTURE_MAPPING_OPACITY 
  PRC_TEXTURE_MAPPING_SPHERICAL_REFLECTION
  */
  
  uint8_t components;
  /*
    PRC_TEXTURE_MAPPING_COMPONENTS_RED
    PRC_TEXTURE_MAPPING_COMPONENTS_GREEN
    PRC_TEXTURE_MAPPING_COMPONENTS_BLUE
    PRC_TEXTURE_MAPPING_COMPONENTS_RGB
    PRC_TEXTURE_MAPPING_COMPONENTS_ALPHA
    PRC_TEXTURE_MAPPING_COMPONENTS_RGBA
  */
  
  EPRCTextureFunction function;
  /*
   enum EPRCTextureFunction {			// Defines how to paint a texture on the surface being rendered.
   KEPRCTextureFunction_Unknown,			// Let the application choose.
   KEPRCTextureFunction_Modulate,		// Combine lighting with texturing. This is the default value.
   KEPRCTextureFunction_Replace,			// Replace the object color with texture color data.
   KEPRCTextureFunction_Blend,			// Reserved for future use.
   KEPRCTextureFunction_Decal			// Reserved for future use.
   };
  */

  EPRCTextureWrappingMode wrapping_mode_S;
  EPRCTextureWrappingMode wrapping_mode_T;
  /*
   enum EPRCTextureWrappingMode {			// Defines repeating and clamping texture modes.
   KEPRCTextureWrappingMode_Unknown,		// Let the application choose.
   KEPRCTextureWrappingMode_Repeat,		// Display the repeated texture on the surface.
   KEPRCTextureWrappingMode_ClampToBorder,	// Clamp the texture to the border. Display the surface color along the texture limits.
   KEPRCTextureWrappingMode_Clamp,		// Reserved for future use.
   KEPRCTextureWrappingMode_ClampToEdge,		// Reserved for future use.
   KEPRCTextureWrappingMode_MirroredRepeat	// Reserved for future use.
   };
  */

};

/*
struct PRCmaterial
{
  PRCmaterial() : alpha(1.0),shininess(1.0),
      picture_data(NULL), picture_format(KEPRCPicture_BITMAP_RGB_BYTE), picture_width(0), picture_height(0), picture_size(0),
      picture_replace(false), picture_repeat(false) {}
  PRCmaterial(const RGBAColour &a, const RGBAColour &d, const RGBAColour &e,
              const RGBAColour &s, double p, double h,
              const uint8_t* pic=NULL, EPRCPictureDataFormat picf=KEPRCPicture_BITMAP_RGB_BYTE,
              uint32_t picw=0, uint32_t pich=0, uint32_t pics=0, bool picreplace=false, bool picrepeat=false) :
      ambient(a), diffuse(d), emissive(e), specular(s), alpha(p), shininess(h),
      picture_data(pic), picture_format(picf), picture_width(picw), picture_height(pich), picture_size(pics),
      picture_replace(picreplace), picture_repeat(picrepeat) {
        if(picture_size==0)
        {
          if (picture_format==KEPRCPicture_BITMAP_RGB_BYTE)
             picture_size = picture_width*picture_height*3;
          if (picture_format==KEPRCPicture_BITMAP_RGBA_BYTE)
             picture_size = picture_width*picture_height*4;
          if (picture_format==KEPRCPicture_BITMAP_GREY_BYTE)
             picture_size = picture_width*picture_height*1;
          if (picture_format==KEPRCPicture_BITMAP_GREYA_BYTE)
             picture_size = picture_width*picture_height*2;
        }
      }
  RGBAColour ambient,diffuse,emissive,specular;
  double alpha,shininess;
  const uint8_t* picture_data;
  EPRCPictureDataFormat picture_format;
  uint32_t picture_width;
  uint32_t picture_height;
  uint32_t picture_size;
  bool picture_replace; // replace material color with texture color? if false - just modify
  bool picture_repeat;  // repeat texture? if false - clamp to edge

  bool operator==(const PRCmaterial &m) const
  {
    return (ambient==m.ambient && diffuse==m.diffuse && emissive==m.emissive
        && specular==m.specular && alpha==m.alpha && shininess==m.shininess
        && picture_replace==m.picture_replace && picture_repeat==m.picture_repeat
        && picture_format==m.picture_format
        && picture_width==m.picture_width && picture_height==m.picture_height && picture_size==m.picture_size
        && (picture_data==m.picture_data || memcmp(picture_data,m.picture_data,picture_size)==0) );
  }
  bool operator<(const PRCmaterial &m) const
  {
    if(ambient!=m.ambient)
      return (ambient<m.ambient);
    if(diffuse!=m.diffuse)
      return (diffuse<m.diffuse);
    if(emissive!=m.emissive)
      return (emissive<m.emissive);
    if(specular!=m.specular)
      return (specular<m.specular);
    if(alpha!=m.alpha)
      return (alpha<m.alpha);
    if(shininess!=m.shininess)
      return (shininess<m.shininess);
    if(picture_replace!=m.picture_replace)
      return (picture_replace<m.picture_replace);
    if(picture_repeat!=m.picture_repeat)
      return (picture_repeat<m.picture_repeat);
    if(picture_format!=m.picture_format)
      return (picture_format<m.picture_format);
    if(picture_width!=m.picture_width)
      return (picture_width<m.picture_width);
    if(picture_height!=m.picture_height)
      return (picture_height<m.picture_height);
    if(picture_size!=m.picture_size)
      return (picture_size<m.picture_size);
    if(picture_data!=m.picture_data)
      return (memcmp(picture_data,m.picture_data,picture_size)<0);
    return false;
  }
};
typedef std::map<PRCmaterial,uint32_t> PRCmaterialMap;
*/

struct PRCtessrectangle // rectangle
{
  PRCVector3d vertices[4];
  uint32_t style;
};
typedef std::vector<PRCtessrectangle> PRCtessrectangleList;

struct PRCtessquad // rectangle
{
  PRCVector3d vertices[4];
  RGBAColour  colours[4];
};
typedef std::vector<PRCtessquad> PRCtessquadList;
/*
struct PRCtesstriangle // textured triangle
{
  PRCtesstriangle() : 
  style(m1) {}
  PRCVector3d vertices[3];
// PRCVector3d normals[3];
// RGBAColour  colors[3];
  PRCVector2d texcoords[3];
  uint32_t style;
};
typedef std::vector<PRCtesstriangle> PRCtesstriangleList;
*/
struct PRCtessline // polyline
{
  std::vector<PRCVector3d> point;
  PRCRgbColor color;
};
typedef std::list<PRCtessline> PRCtesslineList;
typedef std::map<double, PRCtesslineList> PRCtesslineMap;

struct PRCface
{
  PRCface() : transform(NULL), face(NULL) {}
  uint32_t style;
  bool transparent;
  PRCGeneralTransformation3d*  transform;
  PRCFace* face;
};
typedef std::vector <PRCface>  PRCfaceList;

struct PRCcompface
{
  PRCcompface() : face(NULL) {}
  uint32_t style;
  bool transparent;
  PRCCompressedFace* face;
};
typedef std::vector <PRCcompface>  PRCcompfaceList;

struct PRCwire
{
  PRCwire() : style(m1), transform(NULL), curve(NULL) {}
  uint32_t style;
  PRCGeneralTransformation3d*  transform;
  PRCCurve* curve;
};
typedef std::vector <PRCwire>  PRCwireList;

typedef std::map <uint32_t,std::vector<PRCVector3d> >  PRCpointsetMap;

class PRCoptions
{
public:
  double compression;
  double granularity;

  bool closed;   // render the surface as one-sided; may yield faster rendering
  bool tess;     // use tessellated mesh to store straight patches
  bool do_break; //
  bool no_break; // do not render transparent patches as one-faced nodes
  double crease_angle; // crease angle for meshes

  PRCoptions(double compression=0.0, double granularity=0.0, bool closed=false,
             bool tess=false, bool do_break=true, bool no_break=false, double crease_angle=25.8419)
    : compression(compression), granularity(granularity), closed(closed),
      tess(tess), do_break(do_break), no_break(no_break), crease_angle(crease_angle) {}
};

class PRCgroup
{
 public:
  PRCgroup() : 
    product_occurrence(NULL), parent_product_occurrence(NULL), part_definition(NULL), parent_part_definition(NULL), transform(NULL) {}
  PRCgroup(const std::string& name) : 
    product_occurrence(NULL), parent_product_occurrence(NULL), part_definition(NULL), parent_part_definition(NULL), transform(NULL), name(name) {}
  PRCProductOccurrence *product_occurrence, *parent_product_occurrence;
  PRCPartDefinition *part_definition, *parent_part_definition;
  PRCfaceList       faces;
  PRCcompfaceList   compfaces;
  PRCtessrectangleList  rectangles;
// PRCtesstriangleList   triangles;
  PRCtessquadList       quads;
  PRCtesslineMap        lines;
  PRCwireList           wires;
  PRCpointsetMap        points;
  std::vector<PRCPointSet*>      pointsets;
  std::vector<PRCPolyBrepModel*> polymodels;
  std::vector<PRCPolyWire*>      polywires;
  PRCGeneralTransformation3d*  transform;
  std::string name;
  PRCoptions options;
};

void makeFileUUID(PRCUniqueId&);
void makeAppUUID(PRCUniqueId&);

class PRCStartHeader
{
  public:
    uint32_t minimal_version_for_read; // PRCVersion
    uint32_t authoring_version; // PRCVersion
    PRCUniqueId file_structure_uuid;
    PRCUniqueId application_uuid; // should be 0

    PRCStartHeader() :
      minimal_version_for_read(PRCVersion), authoring_version(PRCVersion) {}
    void serializeStartHeader(std::ostream&) const;
    void serializeUncompressedFiles(std::ostream&) const;

    PRCUncompressedFileList uncompressed_files;
    uint32_t getStartHeaderSize() const;
    uint32_t getUncompressedFilesSize() const;
};

class PRCFileStructure : public PRCStartHeader
{
  public:
    uint32_t number_of_referenced_file_structures;
    double tessellation_chord_height_ratio;
    double tessellation_angle_degree;
    std::string default_font_family_name;
    PRCRgbColorList colors;
    PRCRgbColorMap colorMap;
    PRCPictureList pictures;
    PRCPictureMap pictureMap;
    PRCUncompressedFileMap uncompressedfileMap;
    PRCTextureDefinitionList texture_definitions;
    PRCTextureDefinitionMap texturedefinitionMap;
    PRCMaterialList materials;
    PRCMaterialGenericMap materialgenericMap;
    PRCTextureApplicationMap textureapplicationMap;
    PRCStyleList styles;
    PRCStyleMap styleMap;

    PRCCoordinateSystemList reference_coordinate_systems;
    std::vector<PRCFontKeysSameFont> font_keys_of_font;
    PRCPartDefinitionList part_definitions;
    PRCProductOccurrenceList product_occurrences;
//  PRCMarkupList markups;
//  PRCAnnotationItemList annotation_entities;
    double unit;
    PRCTopoContextList contexts;
    PRCTessList tessellations;

    uint32_t sizes[6];
    uint8_t *globals_data;
    PRCbitStream globals_out; // order matters: PRCbitStream must be initialized last
    uint8_t *tree_data;
    PRCbitStream tree_out;
    uint8_t *tessellations_data;
    PRCbitStream tessellations_out;
    uint8_t *geometry_data;
    PRCbitStream geometry_out;
    uint8_t *extraGeometry_data;
    PRCbitStream extraGeometry_out;

    ~PRCFileStructure () {
      for(PRCUncompressedFileList::iterator  it=uncompressed_files.begin();  it!=uncompressed_files.end();  ++it) delete *it;
      for(PRCPictureList::iterator           it=pictures.begin();            it!=pictures.end();            ++it) delete *it;
      for(PRCTextureDefinitionList::iterator it=texture_definitions.begin(); it!=texture_definitions.end(); ++it) delete *it;
      for(PRCMaterialList::iterator          it=materials.begin();           it!=materials.end();           ++it) delete *it;
      for(PRCStyleList::iterator             it=styles.begin();              it!=styles.end();              ++it) delete *it;
      for(PRCTopoContextList::iterator       it=contexts.begin();            it!=contexts.end();            ++it) delete *it;
      for(PRCTessList::iterator              it=tessellations.begin();       it!=tessellations.end();       ++it) delete *it;
      for(PRCPartDefinitionList::iterator    it=part_definitions.begin();    it!=part_definitions.end();    ++it) delete *it;
      for(PRCProductOccurrenceList::iterator it=product_occurrences.begin(); it!=product_occurrences.end(); ++it) delete *it;
      for(PRCCoordinateSystemList::iterator  it=reference_coordinate_systems.begin(); it!=reference_coordinate_systems.end(); it++)
        delete *it;

      free(globals_data);
      free(tree_data);
      free(tessellations_data);
      free(geometry_data);
      free(extraGeometry_data);
    }

    PRCFileStructure() :
      number_of_referenced_file_structures(0),
      tessellation_chord_height_ratio(2000.0),tessellation_angle_degree(40.0),
      default_font_family_name(""),
      unit(1),
      globals_data(NULL),globals_out(globals_data,0),
      tree_data(NULL),tree_out(tree_data,0),
      tessellations_data(NULL),tessellations_out(tessellations_data,0),
      geometry_data(NULL),geometry_out(geometry_data,0),
      extraGeometry_data(NULL),extraGeometry_out(extraGeometry_data,0) {}
    void write(std::ostream&);
    void prepare();
    uint32_t getSize();
    void serializeFileStructureGlobals(PRCbitStream&);
    void serializeFileStructureTree(PRCbitStream&);
    void serializeFileStructureTessellation(PRCbitStream&);
    void serializeFileStructureGeometry(PRCbitStream&);
    void serializeFileStructureExtraGeometry(PRCbitStream&);
    uint32_t addPicture(EPRCPictureDataFormat format, uint32_t size, const uint8_t *picture, uint32_t width=0, uint32_t height=0, std::string name="");
#define ADD_ADDUNIQ( prctype ) \
uint32_t add##prctype( PRC##prctype*& p##prctype ); \
uint32_t add##prctype##Unique( PRC##prctype*& p##prctype);
  
  ADD_ADDUNIQ( UncompressedFile   )
  ADD_ADDUNIQ( Picture            )
  ADD_ADDUNIQ( TextureDefinition  )
  ADD_ADDUNIQ( TextureApplication )
  ADD_ADDUNIQ( MaterialGeneric    )
  ADD_ADDUNIQ( Style              ) 

#undef ADD_ADDUNIQ

    uint32_t addRgbColor(double r, double g, double b);
    uint32_t addRgbColorUnique(double r, double g, double b);
    uint32_t addPartDefinition(PRCPartDefinition*& pPartDefinition);
    uint32_t addProductOccurrence(PRCProductOccurrence*& pProductOccurrence);
    uint32_t addTopoContext(PRCTopoContext*& pTopoContext);
    uint32_t getTopoContext(PRCTopoContext*& pTopoContext);
    uint32_t add3DTess(PRC3DTess*& p3DTess);
    uint32_t add3DWireTess(PRC3DWireTess*& p3DWireTess);
/*
    uint32_t addMarkupTess(PRCMarkupTess*& pMarkupTess);
    uint32_t addMarkup(PRCMarkup*& pMarkup);
    uint32_t addAnnotationItem(PRCAnnotationItem*& pAnnotationItem);
 */
    uint32_t addCoordinateSystem(PRCCoordinateSystem*& pCoordinateSystem);
    uint32_t addCoordinateSystemUnique(PRCCoordinateSystem*& pCoordinateSystem);
};

class PRCFileStructureInformation
{
  public:
    PRCUniqueId UUID;
    uint32_t reserved; // 0
    uint32_t number_of_offsets;
    uint32_t *offsets;

    void write(std::ostream&);

    uint32_t getSize();
};

class PRCHeader : public PRCStartHeader
{
  public :
    uint32_t number_of_file_structures;
    PRCFileStructureInformation *fileStructureInformation;
    uint32_t model_file_offset;
    uint32_t file_size; // not documented

    void write(std::ostream&);
    uint32_t getSize();
};

typedef std::map <PRCGeneralTransformation3d,uint32_t> PRCtransformMap;

class oPRCFile
{
  public:
    oPRCFile(std::ostream &os, double u=1, uint32_t n=1) :
      number_of_file_structures(n),
      fileStructures(new PRCFileStructure*[n]),
      unit(u),
      modelFile_data(NULL),modelFile_out(modelFile_data,0),
      fout(NULL),output(os)
      {
        for(uint32_t i = 0; i < number_of_file_structures; ++i)
        {
          fileStructures[i] = new PRCFileStructure();
          fileStructures[i]->minimal_version_for_read = PRCVersion;
          fileStructures[i]->authoring_version = PRCVersion;
          makeFileUUID(fileStructures[i]->file_structure_uuid);
          makeAppUUID(fileStructures[i]->application_uuid);
          fileStructures[i]->unit = u;
        }

        groups.push(PRCgroup());
        PRCgroup &group = groups.top();
        group.name="root";
        group.transform = NULL;
        group.product_occurrence = new PRCProductOccurrence(group.name);
        group.parent_product_occurrence = NULL;
        group.part_definition = new PRCPartDefinition;
        group.parent_part_definition = NULL;
      }

    oPRCFile(const std::string &name, double u=1, uint32_t n=1) :
      number_of_file_structures(n),
      fileStructures(new PRCFileStructure*[n]),
      unit(u),
      modelFile_data(NULL),modelFile_out(modelFile_data,0),
      fout(new std::ofstream(name.c_str(),
                             std::ios::out|std::ios::binary|std::ios::trunc)),
      output(*fout)
      {
        for(uint32_t i = 0; i < number_of_file_structures; ++i)
        {
          fileStructures[i] = new PRCFileStructure();
          fileStructures[i]->minimal_version_for_read = PRCVersion;
          fileStructures[i]->authoring_version = PRCVersion;
          makeFileUUID(fileStructures[i]->file_structure_uuid);
          makeAppUUID(fileStructures[i]->application_uuid);
          fileStructures[i]->unit = u;
        }

        groups.push(PRCgroup());
        PRCgroup &group = groups.top();
        group.name="root";
        group.transform = NULL;
        group.product_occurrence = new PRCProductOccurrence(group.name);
        group.parent_product_occurrence = NULL;
        group.part_definition = new PRCPartDefinition;
        group.parent_part_definition = NULL;
      }

    ~oPRCFile()
    {
      for(uint32_t i = 0; i < number_of_file_structures; ++i)
        delete fileStructures[i];
      delete[] fileStructures;
      if(fout != NULL)
        delete fout;
      free(modelFile_data);
    }

    void begingroup(const char *name, const PRCoptions *options=NULL,
                    const double* t=NULL);
    void endgroup();

    std::string lastgroupname;
    std::vector<std::string> lastgroupnames;
    std::string calculate_unique_name(const ContentPRCBase *prc_entity,const ContentPRCBase *prc_occurence);
    
    bool finish();
    uint32_t getSize();

    const uint32_t number_of_file_structures;
    PRCFileStructure **fileStructures;
    PRCHeader header;
    PRCUnit unit;
    uint8_t *modelFile_data;
    PRCbitStream modelFile_out; // order matters: PRCbitStream must be initialized last
    PRCmaterialMap materialMap;
    PRCcolourMap colourMap;
    PRCcolourwidthMap colourwidthMap;
    PRCgroup rootGroup;
    PRCtransformMap transformMap;
    std::stack<PRCgroup> groups;
    PRCgroup& findGroup();
    void doGroup(PRCgroup& group);
    uint32_t addColour(const RGBAColour &colour);
    uint32_t addColourWidth(const RGBAColour &colour, double width);
    uint32_t addLineMaterial(const RGBAColour& c, double width)
               { return addColourWidth(c,width); }
    uint32_t addMaterial(const PRCmaterial &material);
    uint32_t addTexturedMaterial(const PRCmaterial &material, uint32_t n=0, const PRCtexture* const* tt=NULL);
    uint32_t addTransform(PRCGeneralTransformation3d*& transform);
    uint32_t addTransform(const double* t);
    uint32_t addTransform(const double origin[3], const double x_axis[3], const double y_axis[3], double scale);
    void addPoint(const double P[3], const RGBAColour &c, double w=1.0);
    void addPoint(double x, double y, double z, const RGBAColour &c, double w);
    void addPoints(uint32_t n, const double P[][3], const RGBAColour &c, double w=1.0);
    void addPoints(uint32_t n, const double P[][3], uint32_t style_index);
    void addLines(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[],
                      const RGBAColour& c, double w,
                      bool segment_color, uint32_t nC, const RGBAColour C[], uint32_t nCI, const uint32_t CI[]);
    uint32_t createLines(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[],
                      bool segment_color, uint32_t nC, const RGBAColour C[], uint32_t nCI, const uint32_t CI[]);
    uint32_t createSegments(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][2],
                            bool segment_color, uint32_t nC, const RGBAColour C[], const uint32_t CI[][2]);

    void addTriangles(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][3], const PRCmaterial& m,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][3],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][3],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][3],
                      uint32_t nM, const PRCmaterial M[], const uint32_t MI[], double ca);
    uint32_t createTriangleMesh(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][3], uint32_t style_index,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][3],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][3],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][3],
                      uint32_t nS, const uint32_t S[], const uint32_t SI[], double ca);
    uint32_t createTriangleMesh(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][3], const PRCmaterial& m,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][3],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][3],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][3],
                      uint32_t nM, const PRCmaterial M[], const uint32_t MI[], double ca)
            {
               const uint32_t style = addMaterial(m);
               if(M!=NULL && nM>0)
               {
                 uint32_t* const styles = new uint32_t[nM];
                 for(uint32_t i=0; i<nM; i++)
                   styles[i]=addMaterial(M[i]);
                 const uint32_t meshid =  createTriangleMesh(nP, P, nI, PI, style, nN, N, NI, nT, T, TI, nC, C, CI, nM, styles, MI, ca);
                 delete[] styles;
                 return meshid;
               }
               else
                 return createTriangleMesh(nP, P, nI, PI, style, nN, N, NI, nT, T, TI, nC, C, CI, 0, NULL, NULL, ca);
            }
    void addQuads(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][4], const PRCmaterial& m,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][4],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][4],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][4],
                      uint32_t nM, const PRCmaterial M[], const uint32_t MI[], double ca);
    uint32_t createQuadMesh(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][4], uint32_t style_index,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][4],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][4],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][4],
                      uint32_t nS, const uint32_t S[],    const uint32_t SI[], double ca);
    uint32_t createQuadMesh(uint32_t nP, const double P[][3], uint32_t nI, const uint32_t PI[][4], const PRCmaterial& m,
                      uint32_t nN, const double N[][3],   const uint32_t NI[][4],
                      uint32_t nT, const double T[][2],   const uint32_t TI[][4],
                      uint32_t nC, const RGBAColour C[],  const uint32_t CI[][4],
                      uint32_t nM, const PRCmaterial M[], const uint32_t MI[], double ca)
            {
               const uint32_t style = addMaterial(m);
               if(M!=NULL && nM>0)
               {
                 uint32_t* const styles = new uint32_t[nM];
                 for(uint32_t i=0; i<nM; i++)
                   styles[i]=addMaterial(M[i]);
                 const uint32_t meshid =  createQuadMesh(nP, P, nI, PI, style, nN, N, NI, nT, T, TI, nC, C, CI, nM, styles, MI, ca);
                 delete[] styles;
                 return meshid;
               }
               else
                 return createQuadMesh(nP, P, nI, PI, style, nN, N, NI, nT, T, TI, nC, C, CI, 0, NULL, NULL, ca);
            }
#define PRCTRANSFORM const double origin[3]=NULL, const double x_axis[3]=NULL, const double y_axis[3]=NULL, double scale=1, const double* t=NULL
#define PRCCARTRANSFORM const double origin[3], const double x_axis[3], const double y_axis[3], double scale
#define PRCGENTRANSFORM const double* t=NULL
#define PRCNOMATERIALINDEX m1
    void useMesh(uint32_t tess_index, uint32_t style_index,            PRCGENTRANSFORM);
    void useMesh(uint32_t tess_index, const PRCmaterial& m,            PRCGENTRANSFORM)
           { useMesh(tess_index,addMaterial(m),t); }
    void useMesh(uint32_t tess_index, uint32_t style_index,            PRCCARTRANSFORM);
    void useMesh(uint32_t tess_index, const PRCmaterial& m,            PRCCARTRANSFORM)
           { useMesh(tess_index,addMaterial(m),origin, x_axis, y_axis, scale); }

    void useLines(uint32_t tess_index, uint32_t style_index,           PRCGENTRANSFORM);
    void useLines(uint32_t tess_index, const RGBAColour& c,  double w, PRCGENTRANSFORM)
           { useLines(tess_index, addLineMaterial(c,w), t); }
    void useLines(uint32_t tess_index, uint32_t style_index,           PRCCARTRANSFORM);
    void useLines(uint32_t tess_index, const RGBAColour& c,  double w, PRCCARTRANSFORM)
           { useLines(tess_index,addLineMaterial(c,w),origin, x_axis, y_axis, scale); }

//  void addTriangle(const double P[][3], const double T[][2], uint32_t style_index);
  
    void addLine(uint32_t n, const double P[][3], const RGBAColour &c, double w=1.0);
    void addSegment(const double P1[3], const double P2[3], const RGBAColour &c, double w=1.0);

    void addBezierCurve(uint32_t n, const double cP[][3], const RGBAColour &c);
    void addCurve(uint32_t d, uint32_t n, const double cP[][3], const double *k, const RGBAColour &c, const double w[]);
    void addQuad(const double P[][3], const RGBAColour C[]);

    void addRectangle(const double P[][3], const PRCmaterial &m);
    void addPatch(const double cP[][3], const PRCmaterial &m);
    void addSurface(uint32_t dU, uint32_t dV, uint32_t nU, uint32_t nV,
     const double cP[][3], const double *kU, const double *kV, const PRCmaterial &m,
     const double w[]);
    void addTube(uint32_t n, const double cP[][3], const double oP[][3], bool straight, const PRCmaterial& m, PRCTRANSFORM);
    void addHemisphere(double radius, const PRCmaterial& m, PRCTRANSFORM);
    void addSphere(double radius, const PRCmaterial& m, PRCTRANSFORM);
    void addDisk(double radius, const PRCmaterial& m, PRCTRANSFORM);
    void addCylinder(double radius, double height, const PRCmaterial& m, PRCTRANSFORM);
    void addCone(double radius, double height, const PRCmaterial& m, PRCTRANSFORM);
    void addTorus(double major_radius, double minor_radius, double angle1, double angle2, const PRCmaterial& m, PRCTRANSFORM);
#undef PRCTRANSFORM
#undef PRCCARTRANSFORM
#undef PRCGENTRANSFORM


    uint32_t addPicture(EPRCPictureDataFormat format, uint32_t size, const uint8_t *picture, uint32_t width=0, uint32_t height=0,
      std::string name="", uint32_t fileStructure=0)
      { return fileStructures[fileStructure]->addPicture(format, size, picture, width, height, name); }

#define ADD_ADDUNIQ( prctype ) \
  uint32_t add##prctype(PRC##prctype*& p##prctype, uint32_t fileStructure=0) \
  { return fileStructures[fileStructure]->add##prctype( p##prctype ); } \
  uint32_t add##prctype##Unique(PRC##prctype*& p##prctype, uint32_t fileStructure=0) \
  { return fileStructures[fileStructure]->add##prctype##Unique(p##prctype); }
  
  ADD_ADDUNIQ( TextureDefinition  )
  ADD_ADDUNIQ( TextureApplication )
  ADD_ADDUNIQ( MaterialGeneric    )
  ADD_ADDUNIQ( Style              ) 
#undef ADD_ADDUNIQ
    uint32_t addRgbColor(double r, double g, double b,
       uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addRgbColor(r, g, b);
      }
    uint32_t addRgbColorUnique(double r, double g, double b,
       uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addRgbColorUnique(r, g, b);
      }
    uint32_t addPartDefinition(PRCPartDefinition*& pPartDefinition, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addPartDefinition(pPartDefinition);
      }
    uint32_t addProductOccurrence(PRCProductOccurrence*& pProductOccurrence, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addProductOccurrence(pProductOccurrence);
      }
    uint32_t addTopoContext(PRCTopoContext*& pTopoContext, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addTopoContext(pTopoContext);
      }
    uint32_t getTopoContext(PRCTopoContext*& pTopoContext, uint32_t fileStructure=0)
    {
      return fileStructures[fileStructure]->getTopoContext(pTopoContext);
    }
    uint32_t add3DTess(PRC3DTess*& p3DTess, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->add3DTess(p3DTess);
      }
    uint32_t add3DWireTess(PRC3DWireTess*& p3DWireTess, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->add3DWireTess(p3DWireTess);
      }
/*
    uint32_t addMarkupTess(PRCMarkupTess*& pMarkupTess, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addMarkupTess(pMarkupTess);
      }
    uint32_t addMarkup(PRCMarkup*& pMarkup, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addMarkup(pMarkup);
      }
    uint32_t addAnnotationItem(PRCAnnotationItem*& pAnnotationItem, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addAnnotationItem(pAnnotationItem);
      }
 */
    uint32_t addCoordinateSystem(PRCCoordinateSystem*& pCoordinateSystem, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addCoordinateSystem(pCoordinateSystem);
      }
    uint32_t addCoordinateSystemUnique(PRCCoordinateSystem*& pCoordinateSystem, uint32_t fileStructure=0)
      {
        return fileStructures[fileStructure]->addCoordinateSystemUnique(pCoordinateSystem);
      }
  private:
    void serializeModelFileData(PRCbitStream&);
    std::ofstream *fout;
    std::ostream &output;
};

#endif // __O_PRC_FILE_H
