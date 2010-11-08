<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/kdoc">
  <html>
  <head><title>KDoc Generated Documentation</title>
  <link rel="stylesheet" href="kdoc.css" type="text/css" />
  </head>
  <body class="kdoc">
    <h1>Top-level Declarations</h1>
      <xsl:for-each select="file">
        <div class="file">
        <h1>
          <xsl:value-of select="name"/>
        </h1>
        <hr/>
        <xsl:apply-templates select="body"/> 
        </div>
      </xsl:for-each>
  </body>
  </html>
</xsl:template>

<xsl:template match="//identifier">
   <a>
	   <xsl:attribute name="href">
        #<xsl:value-of select="."/>
      </xsl:attribute>
      
      <xsl:value-of select="."/>
	</a>
</xsl:template>

<xsl:template match="doc">
   <div class="docstr">
   <p class="description">
      <em><xsl:apply-templates select="description"/></em>
   </p>
   <xsl:apply-templates select="body"/>
   </div>
</xsl:template>

<xsl:template match="usercontent/author">
   <p><strong>Author: </strong> <xsl:value-of select="."/></p>
</xsl:template>

<xsl:template match="dec/typebind">
   <div class="typebind">
   type <xsl:value-of select="tyvars"/> <strong><xsl:value-of select="name"/></strong> = 
      <xsl:apply-templates select="body"/>
   </div>
</xsl:template>

<xsl:template match="dec/valbind">
   <div class="valbind">
   val <strong><xsl:value-of select="name"/></strong>
   </div>
</xsl:template>

<xsl:template match="dec/fun">
   <div class="fundec">
   fun <strong><xsl:value-of select="name"/></strong>
   </div>
</xsl:template>

<xsl:template match="dec/structure">
   <div class="structure">
   <h2>structure <strong><xsl:value-of select="name"/></strong> = <br/>struct</h2>
   <a>
      <xsl:attribute name="name">
        structure.<xsl:value-of select="name"/>
      </xsl:attribute>
   </a>
   <div class="structbody">
   <xsl:apply-templates select="body"/>
   </div>
   <h2>end</h2>
   </div>
</xsl:template>

<xsl:template match="dec/datatype">
   <div class="datatype">
   datatype <strong><xsl:value-of select="name"/></strong> = 
   <ul>
   <xsl:for-each select="body/con">
      <xsl:choose>
         <xsl:when test="type = ''">
            <li><strong><xsl:value-of select="name"/></strong></li> 
         </xsl:when>
         <xsl:otherwise>
            <li><strong><xsl:value-of select="name"/></strong> of 
                <xsl:apply-templates select="type"/></li>
         </xsl:otherwise>
      </xsl:choose>
   </xsl:for-each>
   </ul>
   </div>
</xsl:template>

<xsl:template match="dec/datatypeassign">
   <div class="datatype">
   <h3>datatype <xsl:value-of select="name"/> = 
                <xsl:apply-templates select="body"/></h3>
   </div>
</xsl:template>

<xsl:template match="dec/structureassign">
   <div class="structure">
   structure <xsl:value-of select="name"/> = 
                <xsl:apply-templates select="body"/>
   </div>
</xsl:template>

<xsl:template match="//longid">
   <a>
      <xsl:attribute name="href">
        #structure.<xsl:value-of select="prefix"/>
      </xsl:attribute>
      
      <xsl:value-of select="prefix"/>

   </a>.<a>
      <xsl:attribute name="href">
        #<xsl:value-of select="suffix"/>
      </xsl:attribute>
      
      <xsl:value-of select="suffix"/>
   </a>
</xsl:template>

</xsl:stylesheet>

