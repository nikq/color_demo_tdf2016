#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <assert.h>
#include <string>
#include <map>
#define GLEW_STATIC
#include "include\GL\glew.h"
#include "include\GLFW\glfw3.h"
#include <algorithm>

#define _USE_MATH_DEFINES
#include <math.h>
#include <stdlib.h>
#include <time.h>

#include "ciexyz.h"
#include "stb_image.c"

typedef unsigned long long uint64_t;

float randF( void ) { return rand() / (float)RAND_MAX; }

FILE *logFP = NULL;

void log( const char* str ){
  if(!logFP)
    logFP=fopen("log.txt","wb");
  int l = strlen( str );
  unsigned char crlf = 0x0A;
  fwrite( str, 1, l, logFP );
  fwrite( &crlf, 1, 1, logFP );
  fflush( logFP );
}


// global set
int MAXPASS = 8;
int MAXPATH = 8;
int RAYSIZE = 128;
int ACTIVEBLOCKS = 8;


// GL utils.

class Texture
{
public:
    Texture() { handle_ = 0; }
    Texture( int w, int h, int ch, bool isFloat, bool isLinear, bool isClamped, void *data ) {
        init( w, h, ch, isFloat, isLinear, isClamped, data );
    }
    ~Texture() { clear(); }

    int width_;
    int height_;

    GLuint handle_;
    GLint coordMode_, type_, iformat_, format_;

    GLint  boundUnit_;
    void   *data_;

    void load( const char *fn )
    {
        int n,w,h;
        unsigned char * rgb = stbi_load( fn, &w, &h, &n, 4 );
        assert( rgb );
        init( w, h, n, false, true, true, rgb );
        glGenerateMipmap( GL_TEXTURE_2D );
    }

    void init( int width, int height, int channels, bool isFloat, bool isLinear, bool isClamped, void* data )
    {
        char dbg[ 1024 ];
        _snprintf_s(dbg,1024,1024,"texInit %dx%dx%d %d %d %d %p\n",width,height,channels,isFloat,isLinear,isClamped,data);
        log( dbg );
        coordMode_ = isClamped ? GL_CLAMP_TO_EDGE : GL_REPEAT;
        type_ = isFloat ? GL_FLOAT : GL_UNSIGNED_BYTE;
        GLint iformats[] ={ GL_R32F, GL_RG32F, GL_RGB32F, GL_RGBA32F };
        GLint formats[] ={ GL_LUMINANCE, GL_RG, GL_RGB, GL_RGBA };
        iformat_ = iformats[ channels - 1 ];
        format_ = formats[ channels - 1 ];

        data_ = data;
        width_ = width;
        height_ = height;

        glGenTextures( 1, &handle_ );
        glBindTexture( GL_TEXTURE_2D, handle_ );
        glTexImage2D( GL_TEXTURE_2D, 0, iformat_, width_, height_, 0, format_, type_, data );
        
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, coordMode_ );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, coordMode_ );
        setSmooth( isLinear );
    }
    void copy( void* data )
    {
        //char dbg[ 1024 ];
        /*snprintf( dbg, 1024, "texCopy %dx%dx%d %d %d %p\n", width_, height_, iformat_, format_, type_, data );
        OutputDebugString( dbg ); */
        assert( width_ > 0 );
        glTexImage2D( GL_TEXTURE_2D, 0, iformat_, width_, height_, 0, format_, type_, data );
    }
    void clear( void )
    {
        glDeleteTextures( 1, &handle_ );
    }
    void setSmooth( bool isLinear )
    {
        GLint mode = isLinear ? GL_LINEAR : GL_NEAREST;
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, mode );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, mode );
    }
    void bind( int unit )
    {
        glActiveTexture( GL_TEXTURE0 + unit );
        glBindTexture( GL_TEXTURE_2D, handle_ );
        boundUnit_ = unit;
    }
    void unbind( void )
    {
        glActiveTexture( GL_TEXTURE0 + boundUnit_ );
        glBindTexture( GL_TEXTURE_2D, 0 );
    }
    void flush( void )
    {
    }
};

class RenderTarget
{
public:
    GLuint handle_;
    RenderTarget() { ; }
    ~RenderTarget() { glDeleteFramebuffers( 1, &handle_ ); }

    void init( void )
    {
        //glCreateFramebuffers( 1, &handle_ );
        glGenFramebuffersEXT( 1, &handle_ );
    }
    void bind( void )
    {
        glBindFramebuffer( GL_FRAMEBUFFER, handle_ );
    }
    void unbind( void )
    {
        glBindFramebuffer( GL_FRAMEBUFFER, 0 );
    }
    void attachTexture( Texture& tex, int index )
    {
        glFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + index, GL_TEXTURE_2D, tex.handle_, 0 );
    }
    void detachTexture( int index )
    {
        glFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + index, GL_TEXTURE_2D, 0, 0 );
    }
    void drawBuffers( int numBufs )
    {
        std::vector<GLenum> buffers( numBufs );
        for ( int i = 0; i < numBufs; i++ )
            buffers[ i ] = GL_COLOR_ATTACHMENT0 + i;
        glDrawBuffers( numBufs, &( buffers[ 0 ] ) );
    }
};

class Shader
{
public:

    GLuint program_;
    typedef std::map<std::string, GLint> UniformIndexMap;
    UniformIndexMap uniformIndexMap_;
    std::string fsname_;
    std::string vsname_;
    
    typedef struct { float a_, b_;  } Uniform2F;
    typedef std::map<std::string, float     > UniformUpdateF;
    typedef std::map<std::string, Uniform2F > UniformUpdate2F;

    UniformUpdateF  uniformF_;
    UniformUpdate2F uniform2F_;

    /*
    ** シェーダの情報を表示する
    */
    void printShaderInfoLog( GLuint shader )
    {
        GLsizei bufSize;

        glGetShaderiv( shader, GL_INFO_LOG_LENGTH, &bufSize );

        if ( bufSize > 1 ) {
            GLchar *infoLog;

            infoLog = (GLchar *)malloc( bufSize );
            if ( infoLog != NULL ) {
                GLsizei length;

                glGetShaderInfoLog( shader, bufSize, &length, infoLog );
                char buf[1024];
                _snprintf_s(buf,sizeof(buf),sizeof(buf),"ShaderInfoLog:\n%s\n",infoLog);
                log( buf );
                free( infoLog );
            }
            else
                fprintf( stderr, "Could not allocate InfoLog buffer.\n" );
        }
    }

    /*
    ** プログラムの情報を表示する
    */
    void printProgramInfoLog( GLuint program )
    {
        GLsizei bufSize;

        glGetProgramiv( program, GL_INFO_LOG_LENGTH, &bufSize );

        if ( bufSize > 1 ) {
            GLchar *infoLog;

            infoLog = (GLchar *)malloc( bufSize );
            if ( infoLog != NULL ) {
                GLsizei length;

                glGetProgramInfoLog( program, bufSize, &length, infoLog );
                fprintf( stderr, "InfoLog:\n%s\n\n", infoLog );
                free( infoLog );
            }
            else
                fprintf( stderr, "Could not allocate InfoLog buffer.\n" );
        }
    }

    /*
    ** シェーダーのソースプログラムをメモリに読み込む
    */
    GLuint readShaderSource( GLenum shaderType, const char *file )
    {
        FILE *fp;
        const GLchar *source;
        GLsizei length;
        int ret;

        log(file);
        /* ファイルを開く */
        //fp = fopen_s( file, "rb" );
        int err = fopen_s( &fp, file, "rb" );
        if ( fp == NULL ) {
            perror( file );
            return -1;
        }

        /* ファイルの末尾に移動し現在位置 (つまりファイルサイズ) を得る */
        fseek( fp, 0L, SEEK_END );
        length = ftell( fp );

        /* ファイルサイズのメモリを確保 */
        source = (GLchar *)malloc( length );
        if ( source == NULL ) {
            fprintf( stderr, "Could not allocate read buffer.\n" );
            return -1;
        }

        /* ファイルを先頭から読み込む */
        fseek( fp, 0L, SEEK_SET );
        ret = fread( (void *)source, 1, length, fp ) != (size_t)length;
        fclose( fp );

        GLuint shader = glCreateShader( shaderType );

        /* シェーダのソースプログラムのシェーダオブジェクトへの読み込み */
        if ( ret ){
            char buf[1024];
            _snprintf_s(buf,sizeof(buf),sizeof(buf),"couldnot read file %s\n",file);
            log( buf );
        }
        else
            glShaderSource( shader, 1, &source, &length );

        /* 確保したメモリの開放 */
        free( (void *)source );

        glCompileShader( shader );
        GLint compiled;
        glGetShaderiv( shader, GL_COMPILE_STATUS, &compiled );
        printShaderInfoLog( shader );
        if ( compiled == GL_FALSE ) {
            log( "compile error occured\n" );
            assert( false );
            return ~0;
        }

        return shader;
    }

    Shader() {
        program_ = GL_INVALID_INDEX;
    }
    ~Shader() {
        if( program_ != GL_INVALID_INDEX )
            glDeleteProgram( program_ );
        program_ = GL_INVALID_INDEX;
    }

    void init( const std::string& fragmentShaderFile, const std::string& vertexShaderFile )
    {
        fsname_ = fragmentShaderFile ;
        vsname_ = vertexShaderFile ;

        GLuint fs = readShaderSource( GL_FRAGMENT_SHADER, fragmentShaderFile.c_str() );
        GLuint vs = readShaderSource( GL_VERTEX_SHADER, vertexShaderFile.c_str() );
        program_ = glCreateProgram();
        glAttachShader( program_, fs );
        glAttachShader( program_, vs );
        glLinkProgram( program_ );

        GLint linked;
        glGetProgramiv( program_, GL_LINK_STATUS, &linked );
        printProgramInfoLog( program_ );
        if ( linked == GL_FALSE ) {
            fprintf( stderr, "Link error.\n" );
            assert( false );
        }
        glDeleteShader( fs );
        glDeleteShader( vs );
    }
    void bind( void )
    {
        //printf("use %s + %s\n",fsname_.c_str(),vsname_.c_str());
        assert( program_ != GL_INVALID_INDEX );
        glUseProgram( program_ );
    }


    GLint uniformIndex( const std::string& name )
    {
        UniformIndexMap::const_iterator it = uniformIndexMap_.find( name );
        if ( it != uniformIndexMap_.end() ) {
            // printf( "already known, %s : %d\n", name.c_str(), it->second );
            return it->second;
        }
        GLint index = glGetUniformLocation( program_, name.c_str() );
        //assert( index >= 0 );
        uniformIndexMap_[ name ] = index;
        //printf("unknown %s : %d\n",name.c_str(),index);
        return index;
    }
    void uniformTexture( const std::string& name, const Texture& tex )
    {
        GLint index = uniformIndex( name );
        if( index>=0 )
            glUniform1i( index, tex.boundUnit_ );
    }
    void uniformF( const std::string& name, float f )
    {
        GLint index = uniformIndex( name );
        if ( index >= 0 )
            glUniform1f( index, f );
    }
    void uniform2F( const std::string& name, float f1, float f2 )
    {
        GLint index = uniformIndex( name );
        if ( index >= 0 )
            glUniform2f( index, f1, f2 );
    }

    void clearUniform( void )
    {
        uniformF_.clear();
        uniform2F_.clear();
    }
    void updateUniform( void )
    {
        for ( auto &entry : uniformF_ )
            uniformF( entry.first, entry.second );
        for ( auto &entry : uniform2F_ )
            uniform2F( entry.first, entry.second.a_, entry.second.b_ );
    }
    void updateUniformF( const std::string& name, float f )
    {
        uniformF_[ name ] = f;
    }
    void updateUniform2F( const std::string& name, float a, float b )
    {
        Uniform2F uni;
        uni.a_ = a;
        uni.b_ = b;
        uniform2F_[ name ] = uni;
    }
};


class VertexBuffer
{
public:
    struct Attribute
    {
        std::string name_;
        GLint  size_;
        GLenum type_;
        bool   norm_;
        uint64_t offset_;
        int   index_;
    };
    typedef std::vector<Attribute> AttribList;

    GLuint handle_;
    AttribList list_;
    int elementSize_;
    int length_;

    VertexBuffer()
    {
        clear();
    }
    ~VertexBuffer()
    {
        glDeleteBuffers( 1, &handle_ );
    }

    void clear( void )
    {
        list_.clear();
        elementSize_ = 0;
    }
    void init( int numVerts )
    {
        length_ = numVerts;
        glGenBuffers( 1, &handle_ );
        glBindBuffer( GL_ARRAY_BUFFER, handle_ );
    }
    void copy( void *data )
    {
        glBufferData( GL_ARRAY_BUFFER, elementSize_ * length_, data, GL_STATIC_DRAW );
    }
    void bind( void )
    {
        glBindBuffer( GL_ARRAY_BUFFER, handle_ );
    }
    void unbind( void )
    {
        glBindBuffer( GL_ARRAY_BUFFER, 0 );
    }

    int glTypeSize( GLenum type ) {
        switch ( type ) {
        case GL_BYTE:
        case GL_UNSIGNED_BYTE:
            return 1;
        case GL_SHORT:
        case GL_UNSIGNED_SHORT:
            return 2;
        case GL_INT:
        case GL_UNSIGNED_INT:
        case GL_FLOAT:
            return 4;
        default:
            return 0;
        }
    }

    void addAttrib( std::string& name, GLint size, GLenum type, bool norm )
    {
        Attribute attr;
        attr.name_ = name;
        attr.size_ = size;
        attr.type_ = type;
        attr.norm_ = norm;
        attr.offset_ = elementSize_;
        attr.index_ = -1;
        list_.push_back( attr );
        //printf("%s:%d %d %d %Id\n",name.c_str(),size,type,norm,attr.offset_);
        elementSize_ += size * glTypeSize( type );
    }

    void draw( Shader& shader, GLenum mode, GLint length = 0 )
    {
        //printf("--draw\n");
        for ( int i = 0; i < list_.size(); i++ )
        {
            list_[ i ].index_ = glGetAttribLocation( shader.program_, list_[ i ].name_.c_str() );
            //printf("%s(%s) : %d %Id / %d\n",shader.fsname_.c_str(),list_[i].name_.c_str(),list_[i].index_,list_[i].offset_,elementSize_);
            if ( list_[ i ].index_ >= 0 )
            {
                Attribute& attr( list_[ i ] );
                glEnableVertexAttribArray(list_[i].index_ );
                glVertexAttribPointer(
                    list_[ i ].index_, 
                    list_[ i ].size_, 
                    list_[ i ].type_, 
                    list_[ i ].norm_ ? GL_TRUE : GL_FALSE, 
                    elementSize_,
                    (const void*)( attr.offset_ ) );
            }
        }
        glDrawArrays( mode, 0, length ? length : length_ );
        for ( int i = 0; i < list_.size(); i++ )
        {
            if ( list_[ i ].index_ >= 0 )
            {
                glDisableVertexAttribArray( list_[ i ].index_ );
                list_[ i ].index_ = -1;
            }
        }
    }
};


// implement tantalum.
float vf4rand( float*state0, float*state1 )
{
    const float q[] ={ 1225.0, 1585.0, 2457.0, 2098.0 };
    const float r[] ={ 1112.0, 367.0, 92.0, 265.0 };
    const float a[] ={ 3423.0, 2646.0, 1707.0, 1999.0 };
    const float m[] ={ 4194287.0, 4194277.0, 4194191.0, 4194167.0 };

    float beta[ 4 ];
    for ( int i=0; i < 4; i++ )
        beta[ i ] = floor( state0[ i ] / q[ i ] );
    float p[ 4 ];
    for ( int i=0; i < 4; i++ )
        p[ i ] = a[ i ] * ( state0[ i ] - beta[ i ] * q[ i ] ) - beta[ i ] * r[ i ];
    for ( int i=0; i < 4; i++ ) {
        float s = ( p[ i ] > 0.f ) ? 1.f : ( p[ i ] < 0.f ? -1.f : 0.f );
        beta[ i ] = ( 1.0f - s )*0.5f*m[ i ];
    }
    for ( int i=0; i < 4; i++ )
        state1[ i ] = p[ i ] + beta[ i ];
    float d = ( state1[ 0 ] / m[ 0 ] ) - ( state1[ 1 ] / m[ 1 ] ) + ( state1[ 2 ] / m[ 2 ] ) - ( state1[ 3 ] / m[ 3 ] );
    return d - floor( d );
}


class RayState
{
public:
    int size_;
    std::vector<float> posData_;
    std::vector<float> rngData_;
    std::vector<float> rgbData_;
    Texture posTex_;
    Texture rngTex_;
    Texture rgbTex_;

    void init( int size )
    {
        size_ = size;
        posData_.resize( size_ * size_ * 4 );
        rngData_.resize( size_ * size_ * 4 );
        rgbData_.resize( size_ * size_ * 4 );
        for ( int i = 0; i < size_*size_; i++ )
        {
            float theta = randF() * (float)M_PI * 2.f;
            posData_[ i * 4 + 0 ] = 0.f;
            posData_[ i * 4 + 1 ] = 0.f;
            posData_[ i * 4 + 2 ] = cos( theta );
            posData_[ i * 4 + 3 ] = sin( theta );
            for ( int d = 0; d < 4; d++ )
            {
                rngData_[ i * 4 + d ] = randF() * 4194167.f;
                rgbData_[ i * 4 + d ] = 0.f;
            }
        }
        posTex_.init( size, size, 4, true, false, true, &( posData_[ 0 ] ) );
        rngTex_.init( size, size, 4, true, false, true, &( rngData_[ 0 ] ) );
        rgbTex_.init( size, size, 4, true, false, true, &( rgbData_[ 0 ] ) );
    }
    void bind( Shader& shader )
    {
        posTex_.bind( 0 );
        rngTex_.bind( 1 );
        rgbTex_.bind( 2 );
        shader.uniformTexture( "PosData", posTex_ );
        shader.uniformTexture( "RngData", rngTex_ );
        shader.uniformTexture( "RgbData", rgbTex_ );
    }
    void unbind( void )
    {
        posTex_.unbind();
        rngTex_.unbind();
        rgbTex_.unbind();
    }
    void flush(void)
    {
        posTex_.flush();
        rngTex_.flush();
        rgbTex_.flush();
    }
    void attach( RenderTarget& fbo )
    {
        fbo.attachTexture( posTex_, 0 );
        fbo.attachTexture( rngTex_, 1 );
        fbo.attachTexture( rgbTex_, 2 );
    }
    void detach( RenderTarget& fbo )
    {
        fbo.detachTexture( 0 );
        fbo.detachTexture( 1 );
        fbo.detachTexture( 2 );
    }
};

class TantalumRenderer
{
public:

    enum {
        SPECTRUM_SAMPLES = 256,
        ICDF_SAMPLES     = 1024,
        LAMBDA_MIN = 360,
        LAMBDA_MAX = 750,

        EMISSION_WHITE     = 0,
        EMISSION_BLACKBODY = 1,
        
        EMITTER_POINT = 0,
        EMITTER_AREA  = 1,
        EMITTER_LASER = 2,
    };

    int width_;
    int height_;
    int screen_width_;
    int screen_height_;
    float aspect_;

    int maxPathLength_;
    int raySize_;
    int rayCount_;
    int activeBlocks_;

    int emissionType_;
    int emissionSpread_;

    Shader composeProgram_;
    Shader passProgram_;
    Shader initProgram_;
    Shader rayProgram_;
    Shader traceProgram_;

    Shader blurProgram_;

    Texture spectrumTex_;
    
    std::vector<float> emissionSpectrum_;
    std::vector<float> emissionCdf_;
    std::vector<float> emissionPdf_;
    std::vector<float> emissionIcdf_;

    float   emissionTemperature_; // 国体放射温度.
    Texture emissionTex_;
    Texture emissionIcdfTex_;
    Texture emissionPdfTex_;
    float   exposure_;
    float   blend_;
    float   glare_;
    float   glareRadius_;
    float   glareThreshold_;
    float   whiteOut_;

    int currentState_;
    RayState rayStates_[ 2 ];
    VertexBuffer rayVbo_;
    std::vector<float> rayVboData_;
    RenderTarget fbo_;

    Texture screenBuffer_;
    Texture waveBuffer_;
    Texture blurBuffer_;
    Texture workBuffer_;

    VertexBuffer quadVbo_;
    VertexBuffer screenVbo_; // ディスプレイの真ん中に16:9で書くバッファ.

    int   emitterSpread_;
    float emitterPos_[ 2 ];
    float emitterAngle_;
    float emitterPower_;
    float emitterSpatialSpread_;
    float emitterAngularSpread_[2];
    
    int maxPass_;
    int sampleCount_;

    // ------------------------------------------------------------------

    void setActiveBlocks( int blocks )
    {
        if ( blocks > raySize_ )
            blocks = raySize_;
        if ( blocks < 4 )
            blocks = 4;
        activeBlocks_ = blocks;
    }
    void setMaxPass( int pass )
    {
        maxPass_ = pass;
    }
    void setEmissionType( int type, float temp )
    {
        emissionType_ = type;
        emissionTemperature_ = temp;
        computeEmissionSpectrum( emissionType_ );
    }
    void setMaxPath( int maxPath )
    {
        maxPathLength_ = maxPath;
    }
    void setBlend( float b )
    {
        blend_ = b;
    }
    void setWhiteout( float f )
    {
        whiteOut_ = f;
    }

    void init( int width, int height, int screen_width, int screen_height )
    {
        width_ = width;
        height_=height;
        screen_width_ = screen_width;
        screen_height_= screen_height;
        whiteOut_ = 0.;

        createQuadVbo( quadVbo_ );
        createScreenVbo( screenVbo_ );

        initProgram_.init( "shaders/init_fs.txt", "shaders/init_vs.txt" );
        traceProgram_.init( "shaders/trace_fs.txt", "shaders/trace_vs.txt" );
        rayProgram_.init( "shaders/ray_fs.txt", "shaders/ray_vs.txt" );
        
        //full-quad系.
        composeProgram_.init( "shaders/compose_fs.txt", "shaders/compose_vs.txt" );
        passProgram_.init( "shaders/pass_fs.txt", "shaders/compose_vs.txt" );
        blurProgram_.init( "shaders/blur_fs.txt", "shaders/compose_vs.txt" );

        //int ciexyzTableSize = sizeof( CIEXYZ_31 ) / sizeof( CIEXYZ_31[ 0 ] );
        //printf("cieXyzTablesize %d\n",ciexyzTableSize);
        int tableSize = sizeof( LAMBDA_TO_RGB ) / sizeof( LAMBDA_TO_RGB[ 0 ] );
        spectrumTex_.init( tableSize / 4, 1, 4, true, true, true, LAMBDA_TO_RGB );
        emissionTex_.init( SPECTRUM_SAMPLES, 1, 1, true, false, true, NULL );
        emissionIcdfTex_.init( ICDF_SAMPLES, 1, 1, true, false, true, NULL );
        emissionPdfTex_.init( SPECTRUM_SAMPLES, 1, 1, true, false, true, NULL );

        setActiveBlocks( ACTIVEBLOCKS );
        setMaxPath( MAXPATH );
        setMaxPass( MAXPASS );
        blend_ = 0.5f;
        raySize_ = RAYSIZE;
        rayCount_ = raySize_ * raySize_;
        currentState_ = 0;
        rayStates_[ 0 ].init( raySize_ );
        rayStates_[ 1 ].init( raySize_ );
        rayVbo_.addAttrib( std::string( "TexCoord" ), 3, GL_FLOAT, false );
        rayVbo_.init( rayCount_ );
        rayVboData_.resize( rayCount_ * 2 * 3 );
        for ( int i = 0; i < rayCount_; i++ )
        {
            float u = ( ( i % raySize_ ) + 0.5f ) / (float)raySize_;
            float v = ( (float)floor( i / raySize_ ) + 0.5f ) / (float)raySize_;
            rayVboData_[ i * 6 + 0 ] = u;
            rayVboData_[ i * 6 + 1 ] = v;
            rayVboData_[ i * 6 + 2 ] = 0.f;
            rayVboData_[ i * 6 + 3 ] = u;
            rayVboData_[ i * 6 + 4 ] = v;
            rayVboData_[ i * 6 + 5 ] = 1.f;
        }
        rayVbo_.copy( &( rayVboData_[ 0 ] ) );
        fbo_.init();

        glClearColor( 0, 0, 0, 1 );
        glBlendFunc( GL_ONE, GL_ONE );

        emissionTemperature_ = 6504.f; // D65
        computeEmissionSpectrum( 1 );
        changeResolution( width, height );
        
        float pos[ 2 ],lookat[2];
        pos[ 0 ] = width_ / 2.f;
        pos[ 1 ] = height_ / 2.f;
        lookat[ 0 ] = pos[ 0 ] + 0.1f;
        lookat[ 1 ] = pos[ 1 ] + 0.1f;
        setEmitter( pos, pos, 1.f, 0.f, 0.f );
        setEmissionType( EMISSION_BLACKBODY, 6504.f );

    }

    void setEmitter( float *posA, float *lookat, float power, float angleSpread, float spatialSpread )
    {
        emitterPos_[ 0 ] = posA[ 0 ];
        emitterPos_[ 1 ] = posA[ 1 ];
        emitterAngle_ = (float)atan2( lookat[ 1 ] - posA[ 1 ], lookat[ 0 ] - posA[ 0 ] );
        emitterPower_ = power;
        emitterAngularSpread_[ 0 ] = -emitterAngle_;
        emitterAngularSpread_[ 1 ] = angleSpread;
        emitterSpatialSpread_ = spatialSpread;
    }

    void setGlare( float radius, float threshold, float glare )
    {
        glare_ = glare;
        glareRadius_ = radius;
        glareThreshold_ = threshold;
    }
    
    void computeEmissionSpectrum( int lightType )
    {
        emissionSpectrum_.resize( SPECTRUM_SAMPLES );
        switch ( lightType )
        {
        default:
        case EMISSION_WHITE: // flat white.
            for ( int i=0; i < emissionSpectrum_.size(); i++ )
                emissionSpectrum_[ i ] = 1.f;
            break;
        case EMISSION_BLACKBODY: // blackbody.
            float h  = 6.626070040e-34f;
            float c  = 299792458.0f;
            float kB = 1.3806488e-23f;
            float T  = emissionTemperature_;

            for ( int i = 0; i < SPECTRUM_SAMPLES; ++i ) {
                float l = ( LAMBDA_MIN + ( LAMBDA_MAX - LAMBDA_MIN )*( i + 0.5f ) / (float)SPECTRUM_SAMPLES )*1e-9f;
                float power = 1e-12f * ( 2.0f*h*c*c ) / ( l*l*l*l*l*( (float)exp( h*c / ( l*kB*T ) ) - 1.0f ) );
                emissionSpectrum_[ i ] = power;
            }
            break;
        }
        computeSpectrumIcdf();
        emissionTex_.bind( 0 );
        emissionTex_.copy( &( emissionSpectrum_[ 0 ] ) );
    }
    void computeSpectrumIcdf( void )
    {
        emissionCdf_.resize( SPECTRUM_SAMPLES + 1 );
        emissionPdf_.resize( SPECTRUM_SAMPLES );
        emissionIcdf_.resize( ICDF_SAMPLES );
        float sum = 0.f;
        for ( int i=0; i < SPECTRUM_SAMPLES; i++ )
            sum += emissionSpectrum_[ i ];
        float normalization = 1.f / sum;
        float safety = 0.1f;

        emissionCdf_[ 0 ] = 0.f;
        for ( int i=0; i < SPECTRUM_SAMPLES; i++ )
        {
            emissionSpectrum_[ i ] *= normalization;
            //float observerResponce = CIEXYZ_31[ i * 4 + 1 ]; // XYZのY.
            float observerResponce = ( LAMBDA_TO_RGB[ i * 4 ] + LAMBDA_TO_RGB[ i * 4+1 ] + LAMBDA_TO_RGB[ i * 4 +2] ) / 3.f;
            emissionPdf_[ i ] = observerResponce * ( emissionSpectrum_[ i ] + safety ) / ( 1.f + safety );
            emissionCdf_[ i + 1 ]= emissionPdf_[ i ] + emissionCdf_[ i ];
        }

        float cdfSum = emissionCdf_[ SPECTRUM_SAMPLES ];
        for ( int i=0; i < SPECTRUM_SAMPLES; i++ )
        {
            emissionPdf_[ i ] *= (float)SPECTRUM_SAMPLES / cdfSum;
            emissionCdf_[ i ] /= cdfSum;
        }
        emissionCdf_[ SPECTRUM_SAMPLES ] = 1.f;

        int cdfIndex = 0;
        for ( int i=0; i < ICDF_SAMPLES; i++ )
        {
            float target = min( ( i + 1 ) / (float)ICDF_SAMPLES, 1.f );
            while ( emissionCdf_[ cdfIndex ] < target )
                cdfIndex++;
            float icdf = ( cdfIndex - 1.0f ) / (float)SPECTRUM_SAMPLES;
            //printf( "%d:target %f:%f\n", i, target, icdf );
            emissionIcdf_[ i ] = icdf;
        }
        emissionIcdfTex_.bind(0);
        emissionIcdfTex_.copy( &( emissionIcdf_[ 0 ] ) );
        emissionPdfTex_.bind(0);
        emissionPdfTex_.copy( &( emissionPdf_[ 0 ] ) );
    }

    // スクリーンバッファサイズ.
    void changeResolution( int w, int h )
    {
        width_ = w;
        height_= h;
        aspect_ = width_ / (float)height_;
        screenBuffer_.init( width_, height_, 4, true, true, true, NULL );
        waveBuffer_.init( width_, height_, 4, true, true, true, NULL );
        workBuffer_.init( width_, height_, 4, true, true, true, NULL );
        blurBuffer_.init( width_, height_, 4, true, true, true, NULL );
    }

    void createQuadVbo( VertexBuffer& vbo )
    {
        vbo.addAttrib( std::string("Position"), 3, GL_FLOAT, false );
        vbo.addAttrib( std::string("TexCoord"), 2, GL_FLOAT, false );
        vbo.init( 4 );
        float array[]={
            1.0f,  1.0f, 0.0f, 1.0f, 1.0f,
            -1.0f, 1.0f, 0.0f, 0.0f, 1.0f,
            -1.0f,-1.0f, 0.0f, 0.0f, 0.0f,
            1.0f, -1.0f, 0.0f, 1.0f, 0.0f,
        };
        vbo.copy( array );
    }
    void createScreenVbo( VertexBuffer& vbo )
    {
        vbo.addAttrib( std::string( "Position" ), 3, GL_FLOAT, false );
        vbo.addAttrib( std::string( "TexCoord" ), 2, GL_FLOAT, false );
        vbo.init( 4 );
        
        float w = 1.f;//(float)width_  / (float)screen_width_ ;
        float h = ((float)height_ / width_)/ ((float)screen_height_ / screen_width_) ;
        float array[]={
            w,  h, 0.0f, 1.0f, 1.0f,
            -w, h, 0.0f, 0.0f, 1.0f,
            -w, -h, 0.0f, 0.0f, 0.0f,
            w,  -h, 0.0f, 1.0f, 0.0f,
        };
        vbo.copy( array );
    }


    void glare_blur( Texture& src, Texture& dst, float Radius, float threshold )
    {
        fbo_.drawBuffers( 1 );
        fbo_.attachTexture( workBuffer_, 0 );
        glClear( GL_COLOR_BUFFER_BIT );
        blurProgram_.bind();
        src.bind(0);
        blurProgram_.uniformTexture( "Frame", src );
        blurProgram_.uniform2F( "FrameSize", (float)src.width_, (float)src.height_);
        blurProgram_.uniform2F( "Dir", 1.0f, 0.0f );
        blurProgram_.uniformF( "Radius", Radius );
        blurProgram_.uniformF( "Threshold", threshold );
        quadVbo_.draw( blurProgram_, GL_TRIANGLE_FAN );
        src.unbind();
        fbo_.detachTexture( 0 );
        fbo_.attachTexture( dst, 0 );
        glClear( GL_COLOR_BUFFER_BIT );
        workBuffer_.bind(0);
        blurProgram_.uniformTexture( "Frame", workBuffer_ );
        blurProgram_.uniform2F( "FrameSize", (float)workBuffer_.width_, (float)workBuffer_.height_);
        blurProgram_.uniform2F( "Dir", 0.0f, 1.0f );
        blurProgram_.uniformF( "Radius", Radius );
        blurProgram_.uniformF( "Threshold", threshold );
        quadVbo_.draw( blurProgram_, GL_TRIANGLE_FAN );
        fbo_.detachTexture( 0 );
    }

    void composite( void )
    {
        if( glare_ > 0.f )
            glare_blur( screenBuffer_, blurBuffer_, glareRadius_, glareThreshold_ );
        fbo_.unbind(); // display
        if( blend_ < 1.f )
        {
            glEnable( GL_BLEND );
            glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        }
        screenBuffer_.bind( 0 );
        blurBuffer_.bind( 1 );
        composeProgram_.bind();
        composeProgram_.uniformTexture( "Frame", screenBuffer_ );
        composeProgram_.uniformTexture( "BlurFrame", blurBuffer_ );
        composeProgram_.uniformF( "Exposure", exposure_ * width_ / (float)sampleCount_);
        composeProgram_.uniformF( "Glare", glare_ );
        composeProgram_.uniformF( "Blend", blend_ );
        composeProgram_.uniformF( "Whiteout", whiteOut_ );
        glViewport( 0, 0, screen_width_, screen_height_ );
        screenVbo_.bind();
        screenVbo_.draw( composeProgram_, GL_TRIANGLE_FAN );
        
        if( blend_ < 1.f )
            glDisable( GL_BLEND );
    }

    void render( Shader& traceShader )
    {
        int current = currentState_;
        int next    = 1 - current;
        sampleCount_ = 0;

        for ( int pass=0; pass < maxPass_; pass++ )
        {
            fbo_.bind();
            glViewport( 0, 0, raySize_, raySize_ );
            glScissor( 0, 0, raySize_, activeBlocks_ );
            glEnable( GL_SCISSOR_TEST );

            // ray初期化.
            fbo_.drawBuffers( 3 );
            rayStates_[ next ].attach( fbo_ ); // 描画先はrayStates_[next]
            initProgram_.bind();
            rayStates_[ current ].rngTex_.bind( 0 );
            spectrumTex_.bind( 1 );
            emissionTex_.bind( 2 );
            emissionIcdfTex_.bind( 3 );
            emissionPdfTex_.bind( 4 );
            initProgram_.uniformTexture( "RngData", rayStates_[ current ].rngTex_ );
            initProgram_.uniformTexture( "Spectrum", spectrumTex_ );
            initProgram_.uniformTexture( "Emission", emissionTex_ );
            initProgram_.uniformTexture( "ICDF_tex", emissionIcdfTex_ );
            initProgram_.uniformTexture( "PDF_tex", emissionPdfTex_ );
            initProgram_.uniform2F( "EmitterPos", ( ( emitterPos_[ 0 ] / width_ )*2.0f - 1.0f )*aspect_, 1.0f - ( emitterPos_[ 1 ] / height_ )*2.0f );
            initProgram_.uniform2F( "EmitterDir", -cosf( emitterAngle_ ), sinf( emitterAngle_ ) );
            initProgram_.uniformF( "EmitterPower", emitterPower_ );
            initProgram_.uniformF( "SpatialSpread", emitterSpatialSpread_ );
            initProgram_.uniform2F( "AngularSpread", emitterAngularSpread_[ 0 ], emitterAngularSpread_[ 1 ] );
            quadVbo_.bind();
            quadVbo_.draw( initProgram_, GL_TRIANGLE_FAN );
            spectrumTex_.unbind();
            emissionTex_.unbind();
            emissionIcdfTex_.unbind();
            emissionPdfTex_.unbind();
            rayStates_[ next ].flush();
            glDisable( GL_SCISSOR_TEST );

            for ( int pathLength =0; pathLength < maxPathLength_; pathLength++ )
            {
                // rayをトレースする. current>next.
                {
                    glViewport( 0, 0, raySize_, raySize_ );
                    glScissor( 0, 0, raySize_, activeBlocks_ );
                    glEnable( GL_SCISSOR_TEST );
                    next = current;
                    current = 1 - current;
                    fbo_.drawBuffers( 3 );
                    rayStates_[ next ].attach( fbo_ );

                    traceShader.bind();
                    traceShader.updateUniform();
                    rayStates_[ current ].bind( traceShader );

                    quadVbo_.bind();
                    quadVbo_.draw( traceShader, GL_TRIANGLE_FAN );
                    rayStates_[ current ].unbind();
                    rayStates_[ next ].detach( fbo_ );
                    rayStates_[ next ].flush();
                    glDisable( GL_SCISSOR_TEST );
                }

                // rayを描画する. 描画先はwaveBuffer_
                glViewport( 0, 0, width_, height_ );
                fbo_.drawBuffers( 1 );
                fbo_.attachTexture( waveBuffer_, 0 );

                if ( pathLength == 0 )
                    glClear( GL_COLOR_BUFFER_BIT );

                // 加算していく.
                glEnable( GL_BLEND );
                glBlendFunc( GL_ONE,GL_ONE);
                rayStates_[ current ].posTex_.bind( 0 );
                rayStates_[ next ].posTex_.bind( 1 );
                rayStates_[ current ].rgbTex_.bind( 2 );
                rayProgram_.bind();
                rayProgram_.uniformTexture( "PosDataA", rayStates_[ current ].posTex_ );
                rayProgram_.uniformTexture( "PosDataB", rayStates_[ next ].posTex_ );
                rayProgram_.uniformTexture( "RgbData", rayStates_[ current ].rgbTex_ );
                rayProgram_.uniformF( "Aspect", aspect_ );
                rayVbo_.bind();
                rayVbo_.draw( rayProgram_, GL_LINES, raySize_*activeBlocks_ * 2 );
                rayStates_[ current ].posTex_.unbind();
                rayStates_[ next ].posTex_.unbind();
                rayStates_[ current ].rgbTex_.unbind();
                glDisable( GL_BLEND );
                
                sampleCount_ += raySize_ * activeBlocks_;
            }
            
            waveBuffer_.flush();
            quadVbo_.bind();
            fbo_.drawBuffers( 1 );
            fbo_.attachTexture( screenBuffer_, 0 );
            if ( pass == 0 )
                glClear( GL_COLOR_BUFFER_BIT );
            glEnable( GL_BLEND );
            glBlendFunc( GL_ONE, GL_ONE );
            passProgram_.bind();
            waveBuffer_.bind( 0 );
            passProgram_.uniformTexture( "Frame", waveBuffer_ );
            quadVbo_.draw( passProgram_, GL_TRIANGLE_FAN );
            screenBuffer_.flush();
            glDisable( GL_BLEND );
        }
        composite(); // render to display buffer.
        currentState_ = next;
    }
};


/*------------------------------*
** デバッグsourceを文字列に変換
**------------------------------*/
const char *debug_source_to_string( GLenum source )
{
    switch ( source ) {
    case GL_DEBUG_SOURCE_API:
        return "OpenGL";
    case GL_DEBUG_SOURCE_WINDOW_SYSTEM:
        return "Window system";
    case GL_DEBUG_SOURCE_THIRD_PARTY:
        return "Third party";
    case GL_DEBUG_SOURCE_APPLICATION:
        return "Application";
    case GL_DEBUG_SOURCE_OTHER:
        return "Other";
    default:
        return "Unknown";
    }
}

/*------------------------------*
** デバッグtypeを文字列に変換
**------------------------------*/
const char *debug_type_to_string( GLenum type )
{
    switch ( type ) {
    case GL_DEBUG_TYPE_ERROR:
        return "Error";
    case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR:
        return "Deprecated behavior";
    case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:
        return "Undefined behavior";
    case GL_DEBUG_TYPE_PORTABILITY:
        return "Portability";
    case GL_DEBUG_TYPE_PERFORMANCE:
        return "Performance";
    case GL_DEBUG_TYPE_MARKER:
        return "Marker";
    case GL_DEBUG_TYPE_PUSH_GROUP:
        return "Push group";
    case GL_DEBUG_TYPE_POP_GROUP:
        return "Pop group";
    case GL_DEBUG_TYPE_OTHER:
        return "Other";
    default:
        return "Unknown";
    }
}

/*------------------------------*
** デバッグseverityを文字列に変換
**------------------------------*/
const char *debug_severity_to_string( GLenum severity )
{
    switch ( severity ) {
    case GL_DEBUG_SEVERITY_HIGH:
        return "High";
    case GL_DEBUG_SEVERITY_MEDIUM:
        return "Medium";
    case GL_DEBUG_SEVERITY_LOW:
        return "Low";
    case GL_DEBUG_SEVERITY_NOTIFICATION:
        return "Notification";
    default:
        return "Unknown";
    }
}
void APIENTRY opengl_debug_callback(
    GLenum source, GLenum type, GLuint id,
    GLenum severity, GLsizei length,
    const GLchar *message, const void *param )
{
    const char *source_str = debug_source_to_string( source );
    const char *type_str = debug_type_to_string( type );
    const char *severity_str = debug_severity_to_string( severity );

    char buffer[ 4096 ];
    _snprintf_s( buffer, 4096, 4096, "GLERR %s:%s[%s](%d): %s\n", source_str, type_str, severity_str, id, message );
    log( buffer );
}


void glfw_error_callback_func( int error, const char* description ) {
    char buffer[4096];
    _snprintf_s( buffer, 4096, 4096, "GLFW Error: %d : %s\n", error, description );
    log( buffer );
}


#define SCENE_BPM                128.f
#define SCENE_TIME(PAGE_INDEX) (((PAGE_INDEX-1.f)*4.f/(float)SCENE_BPM)*60.f) // 4beats 1page in second

float mix( float a, float b, float ratio )
{
    if ( ratio < 0. )
        return a;
    if ( ratio > 1. )
        return b;
    return a + ( b - a )*ratio;
}

class SceneManager
{
public:    
   
    TantalumRenderer render_;

    float bpm_;
    float offset_;
    int scene_index_;
    bool finished_;

    Shader texquadShader_;
    Texture logoTex_; // col.or.
    Texture pathTex_; // pathtracing
    Texture blackTex_; // blackbody
    Texture cauchyTex_; // cauchy
    Texture greetTex_;
    VertexBuffer texquadVbo_;
    typedef struct {
        float x, y, z, u, v;
    } TexquadVertex;
    TexquadVertex logoVertex_[ 4 ];


    class Scene{
    public:
        float timing_;
        const char *fs_;
        const char *vs_;
        Shader traceShader_;
        bool enabled_;

        Scene( float t, const char *fs, const char *vs ) { 
            timing_ = t; 
            enabled_ = false;
            fs_ = fs;
            vs_ = vs;
            if ( fs && vs ) {
                enabled_ = true;
                char buf[1024];
                _snprintf_s(buf,sizeof(buf),sizeof(buf),"%s+%s\n",fs,vs);
                log( buf );
            }
        }
        void init( void )
        {
            if ( enabled_ )
            {
                traceShader_.init( std::string( fs_ ), std::string( vs_ ) );
            }
        }

        bool enabled() const { return enabled_; }
        Shader& shader() { return traceShader_; }
    };

    std::vector< Scene > scenes_;
    
    SceneManager() {
        scenes_.push_back( Scene( SCENE_TIME( 1.f ), "shaders/scene_0.txt", "shaders/trace_vs.txt" ) );  // LOADING
        scenes_.back().shader().updateUniform2F( "BoxSize", 1, 1 );
        scenes_.push_back( Scene( SCENE_TIME( 2.f ), "shaders/scene_1.txt", "shaders/trace_vs.txt" ) );  // BOX+SPHERE, LIGHT POSITION TRANSITION.
        scenes_.back().shader().updateUniform2F( "BoxSize", 1, 1 );
        scenes_.back().shader().updateUniform2F( "SphereMat", 1, 10 );
        scenes_.push_back( Scene( SCENE_TIME( 10.f ), "shaders/scene_2.txt", "shaders/trace_vs.txt" ) );  // BOX+SPHERE, LIGHT BEAM, DISPERSION.
        scenes_.back().shader().updateUniform2F( "BoxSize", 1, 1 );
        scenes_.back().shader().updateUniform2F( "SphereMat", 1, 10 );
        scenes_.push_back( Scene( SCENE_TIME( 18.f ), "shaders/scene_3.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.push_back( Scene( SCENE_TIME( 26.f ), "shaders/scene_4.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.push_back( Scene( SCENE_TIME( 34.f ), "shaders/scene_5.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.back().shader().updateUniform2F( "PrismRotation", 1, 0 );
        scenes_.back().shader().updateUniform2F( "PrismScale", 1, 1 );
        scenes_.push_back( Scene( SCENE_TIME( 42.f ), "shaders/scene_6.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.back().shader().updateUniform2F( "PrismRotation", 1, 0 );
        scenes_.back().shader().updateUniform2F( "PrismScale", 1, 1 );
        scenes_.push_back( Scene( SCENE_TIME( 50.f ), "shaders/scene_6.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.back().shader().updateUniform2F( "PrismRotation", 1, 0 );
        scenes_.back().shader().updateUniform2F( "PrismScale", 1, 1 );
        // scenes_.push_back( Scene( SCENE_TIME( 100000.f ), "shaders/trace_fs.txt", "shaders/trace_vs.txt" ) );  // LENSES + TITLE
        scenes_.push_back( Scene( SCENE_TIME( 54.f ),NULL,NULL ) ); // FINITO
        for ( auto &a : scenes_ )
            a.init();
    }

    float prev_;

    void init( int w,int h, int sw, int sh, float bpm, float offset = 0.f )
    {
        logoTex_.load( "textures/TITLE.png" );
        pathTex_.load( "textures/pathtrace.png" );
        blackTex_.load( "textures/blackbody.png" );
        cauchyTex_.load( "textures/cauchy.png" );
        greetTex_.load( "textures/greet.png" );
        texquadShader_.init( "shaders/texquad_fs.txt", "shaders/texquad_vs.txt" );
        texquadVbo_.addAttrib( std::string( "Position" ), 3, GL_FLOAT, false );
        texquadVbo_.addAttrib( std::string( "TexCoord" ), 2, GL_FLOAT, false );
        texquadVbo_.init( 4 );
        
        logoVertex_[ 0 ].x = 1.0f;
        logoVertex_[ 0 ].y = 0.5f;
        logoVertex_[ 0 ].z = 0.0f;
        logoVertex_[ 0 ].u = 1.0f;
        logoVertex_[ 0 ].v = 0.0f;

        logoVertex_[ 1 ].x = -1.0f;
        logoVertex_[ 1 ].y = 0.5f;
        logoVertex_[ 1 ].z = 0.0f;
        logoVertex_[ 1 ].u = 0.0f;
        logoVertex_[ 1 ].v = 0.0f;

        logoVertex_[ 2 ].x = -1.0f;
        logoVertex_[ 2 ].y = -0.5f;
        logoVertex_[ 2 ].z = 0.0f;
        logoVertex_[ 2 ].u = 0.0f;
        logoVertex_[ 2 ].v = 1.0f;

        logoVertex_[ 3 ].x = 1.0f;
        logoVertex_[ 3 ].y = -0.5f;
        logoVertex_[ 3 ].z = 0.0f;
        logoVertex_[ 3 ].u = 1.0f;
        logoVertex_[ 3 ].v = 1.0f;
        texquadVbo_.copy( &(logoVertex_[0]) );

        render_.init( w, h, sw, sh );
        bpm_ = bpm;
        offset_ = offset;
        scene_index_ = 0;
        finished_ = false;
        prev_ = -1.f;
    }

    void render( float second )
    {
        float minute = second / 60.0f;
        float beat   = minute * bpm_;
        float step   = fmod( beat, 4.f );
        // シーン割合
        if ( second >= scenes_[ scene_index_ + 1 ].timing_ )
            scene_index_++;
        if ( scene_index_ + 1 >= scenes_.size() )
        {
            finished_ = true;
            return;
        }

        float diff = second - prev_;
        //render_.setActiveBlocks( 12 );
        prev_ = second;

        float ratio = ( second - scenes_[ scene_index_ ].timing_ ) / ( scenes_[ scene_index_ + 1 ].timing_ - scenes_[ scene_index_ ].timing_ );

        //printf("%f sec, scene %d : ratio %f\n",second,scene_index_,ratio);
        float emitterPos[ 2 ], emitterLookAt[ 2 ];
        float w = (float)render_.width_;
        float h = (float)render_.height_;
        
        render_.setGlare( 10.f, 0.5, ratio );
        Scene& scene( scenes_[ scene_index_ ] );
        switch ( scene_index_ )
        {
        case 0:{
            emitterPos[ 0 ] = w * 0.5f;
            emitterPos[ 1 ] = h * 0.5f;
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = h / 2.f;
            render_.exposure_ = ratio;
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, M_PI * 2.f, 0.0f ); // LASER SPREAD
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            render_.render( scene.shader() );
        }break;
        case 1: {
            emitterPos[ 0 ] = w * 0.5f * (1.f - ratio);
            emitterPos[ 1 ] = h * 0.5f;
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = h / 2.f;
            render_.exposure_ = 1.f;
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, M_PI * 2.f * (1.f - ratio), 0.0f ); // LASER SPREAD
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            scene.shader().updateUniform2F( "BoxSize", 1.0f + 0.78f*ratio, 1.0f );
            scene.shader().updateUniform2F( "SphereMat", 1.0f + 0.8f*ratio, 10.0f );
            render_.render( scene.shader() );

            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                logoTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture", logoTex_ );
                texquadShader_.uniformF( "Blend", mix( 0.1f, 0.f, ratio*4.f ) );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }

        }break;
        case 2: {
            emitterPos[ 0 ] = 0.f;
            emitterPos[ 1 ] = mix( h * 0.5f, h*0.3f, ratio );
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = mix( h * 0.5f, h*0.33f, ratio );
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, 0.f, mix(0.0f,0.01f,ratio) ); // BEAM SPREAD
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            scene.shader().updateUniform2F( "BoxSize", 1.78f, 1.0f );
            scene.shader().updateUniform2F( "SphereMat", 1.8f, 60.0f - 50.f * ratio );
            render_.render( scene.shader() );

            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                pathTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture", pathTex_ );
                texquadShader_.uniformF( "Blend", mix( 0.5f, 0.f, ratio*4.f ) );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }

        }break;
        case 3: {
            emitterPos[ 0 ] = 0.f; 
            emitterPos[ 1 ] = mix( h * 0.33f, h*0.6f, ratio );
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = mix( h * 0.33f, h*0.6f, ratio );
            render_.setMaxPath( 4 + 8 * ratio );
            render_.exposure_ = 1.f;
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, mix(0.f,M_PI,ratio), 0.f );
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            render_.render( scene.shader() );
            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                cauchyTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture", cauchyTex_ );
                texquadShader_.uniformF( "Blend", mix( 0.5f, 0.f, ratio*4.f ) );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }

        }break;
        case 4: {
            // TDF logo shown!
            emitterPos[ 0 ] = mix( w * 0.25f, w*0.75f, ratio );
            emitterPos[ 1 ] = h * 0.1f;
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = h / 2.f;
            render_.setMaxPath( 8 );
            render_.exposure_ = 1.f;
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, 1, 0.4f );
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, mix( 6504.f, 3500.f, ratio ) );
            render_.render( scene.shader() );
            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                blackTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture", blackTex_ );
                texquadShader_.uniformF( "Blend", mix( 0.2f, 0.f, ratio*4.f ) );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }

        }break;
        case 5: {
            // クロスダイクロプリズムくるくる
            float r2 = pow( ratio, 2.f );
            emitterPos[ 0 ] = 0.f;
            emitterPos[ 1 ] = mix( h * 0.7f, h*0.7f, ratio );
            emitterLookAt[ 0 ] = w / 2.f;
            emitterLookAt[ 1 ] = mix( h * 0.7f, h*0.4f, r2 );
            render_.setMaxPath( 8 );
            render_.exposure_ = mix( 1, 2, ratio );
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, mix(0.2f,0.01f,r2), mix( 0.2f, 0.01f, ratio ) );
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, mix( 3500.f, 6504.f, ratio ) );
            // render_.setBlend( mix( 0.5f, 0.9f, ratio) );
            render_.setBlend( 1.f );
            scene.shader().updateUniform2F( "PrismRotation", cosf( r2 * M_PI * 16.f ), sinf( r2 * M_PI * 16.f ) );
            scene.shader().updateUniform2F( "PrismScale", mix(0.2f, 1.f, r2) , mix(0.2f, 1.f, r2) );
            render_.setWhiteout( r2 );
            render_.render( scene.shader() );
            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                greetTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture",greetTex_ );
                float f = 1.;
                if ( ratio < 0.5f )
                    f = fmod( ratio * 32.f, 1.f );
                else if( ratio < 0.75f )
                    f = fmod( (ratio - 0.5f) * 32.f, 1.f );
                else if( ratio < 0.875f )
                    f = fmod( ( ratio - 0.75f ) * 64.f, 1.f );
                else
                    f = fmod( ( ratio - 0.875f) * 128.f, 1.f );
                texquadShader_.uniformF( "Blend", f );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }
        }break;
        case 6: {
            render_.setWhiteout( mix(1.f,0.f,ratio*1024.f) );
            emitterPos[ 0 ] = 0.f;
            emitterPos[ 1 ] = mix( h * 0.7f, h*0.5f, ratio );
            emitterLookAt[ 0 ] = w * 0.5f;
            emitterLookAt[ 1 ] = h * 0.4f;
            render_.setBlend( mix(1.0f,0.1f,ratio) );
            render_.exposure_ = mix( 1, 2, ratio );
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, 0.f, 0.f);
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            scene.shader().updateUniform2F( "PrismRotation", 1.0f, 0.0f );
            scene.shader().updateUniform2F( "PrismScale", 1.0f,1.0f );
            render_.render( scene.shader() );
            
            {
                glEnable( GL_BLEND );
                glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
                texquadShader_.bind();
                logoTex_.bind( 1 );
                texquadShader_.uniformTexture( "Texture", logoTex_ );
                texquadShader_.uniformF( "Blend", mix( 1.f, 0.f, ratio ) );
                texquadVbo_.bind();
                texquadVbo_.draw( texquadShader_, GL_TRIANGLE_FAN );
                glDisable( GL_BLEND );
            }

        }break;
        case 7: {
            emitterPos[ 0 ] = 0.f;
            emitterPos[ 1 ] = h * 0.5f;
            emitterLookAt[ 0 ] = w * 0.5f;
            emitterLookAt[ 1 ] = h * 0.4f;
            render_.exposure_ = mix( 2, 0, pow(ratio,0.5) );
            render_.setEmitter( emitterPos, emitterLookAt, 10.f, mix(0.f,M_PI,ratio), mix(0.f, 2.f, ratio ) );
            render_.setEmissionType( TantalumRenderer::EMISSION_BLACKBODY, 6504.f );
            scene.shader().updateUniform2F( "PrismRotation", 1.0f, 0.0f );
            float size = mix( 1.f, 0.8f, ratio );
            scene.shader().updateUniform2F( "PrismScale", size, size  );
            render_.render( scene.shader() );
        }break;
        default:
            break;
        }
    }
};



/*
** メインプログラム
*/
int main( int argc, char *argv[] )
{
    bool fullscreen = true;
    int request_width  = 1920;
    int request_height = request_width * 9 / 16;
    
    for(int i=1;i<argc;i++)
    {
        if( argv[i][0] == '-' )
        {
            if( !strcmp( argv[i]+1, "windowed" ) )
                fullscreen = false;
            else if( !strcmp( argv[i]+1, "resoX" ) ){
                request_width  = atoi( argv[i+1] );
                request_height = request_width * 9 / 16;
            }else if( !strcmp( argv[i]+1, "resoY" ) ){
                request_height = atoi( argv[i+1] );
            }else if( !strcmp( argv[i]+1, "RAYSIZE" ) ){
                RAYSIZE = atoi( argv[i+1] );
            }else if( !strcmp( argv[i]+1, "ACTIVEBLOCKS" ) ){
                ACTIVEBLOCKS = atoi( argv[i+1] );
            }else if( !strcmp( argv[i]+1, "MAXPASS" ) ){
                MAXPASS = atoi( argv[i+1] );
            }else if( !strcmp( argv[i]+1, "MAXPATH" ) ){
                MAXPATH = atoi( argv[i+1] );
            }
        }
    }
    
    glfwSetErrorCallback( glfw_error_callback_func );
    glfwInit();
    glfwWindowHint( GLFW_STENCIL_BITS, 0 );
    glfwWindowHint( GLFW_DEPTH_BITS, 0 );

    GLFWwindow *window = glfwCreateWindow( request_width, request_height , "color", fullscreen ? glfwGetPrimaryMonitor() : NULL, NULL );

    int width, height;
    glfwGetFramebufferSize( window, &width, &height );
    printf("%dx%d>%dx%d\n",request_width,request_height,width,height);

    assert( window );
    glfwMakeContextCurrent( window );
    
    GLenum ret = glewInit();
    assert( ret == GLEW_OK );
    
    glDebugMessageCallback( opengl_debug_callback , NULL );
    glEnable( GL_DEBUG_OUTPUT );
    printf("initializing..\n");

    SceneManager demo;
    // demo.init( request_width, request_height, width, height, 128.0 );
    // demo.init( request_width, request_height, width, height, 128.0 );
    int off_width = width / 2;
    int off_height= off_width * 9/ 16;
    demo.init( off_width, off_height, width, height, 128.0 );

    if ( fullscreen )
        glfwSetInputMode( window, GLFW_CURSOR, GLFW_CURSOR_HIDDEN );
    
    // 曲スタート
    MCI_OPEN_PARMS mciOpenParam;
    MCI_PLAY_PARMS mciPlayParam;

    //mciOpenParam.lpstrDeviceType = ( LPSTR )"WaveAudio";
    //mciOpenParam.lpstrElementName= ( LPSTR )"bgm.wav    ";
    mciOpenParam.lpstrDeviceType = ( LPSTR )"MPEGVideo";
    mciOpenParam.lpstrElementName= ( LPSTR )"bgm.mp3";
    MCIERROR err = mciSendCommand( 0, MCI_OPEN, MCI_OPEN_TYPE | MCI_OPEN_ELEMENT, (DWORD_PTR)&mciOpenParam );
    char str[ 1024 ];
    mciGetErrorString(
        err,
        str, sizeof(str)
        );
    printf( "err %x : %s\n", err,str );
    err = mciSendCommand( mciOpenParam.wDeviceID, MCI_PLAY, MCI_NOTIFY, (DWORD_PTR)&mciPlayParam );
    mciGetErrorString(
        err,
        str, sizeof( str )
        );
    printf( "err %x : %s\n", err, str ); 
    // play music!!
    glfwSetTime( 0. ); // reset timer!


    while ( !glfwWindowShouldClose( window ) )
    {
        demo.render( (float)glfwGetTime() );
        if ( demo.finished_ )
            break;
        glfwSwapBuffers( window );
        glfwPollEvents();
        Sleep( 1 );
    }
    glfwTerminate();
    
    return 0;
}