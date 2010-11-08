<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/kdoc">
  <html>
  <head><title>KDoc Generated Documentation</title></head>
  <body>
    <h1>Top-level Declarations</h1>
      <xsl:for-each select="file">
        <h1>
          <xsl:value-of select="name"/>
        </h1>
        <hr/>
        <xsl:apply-templates select="doc"/>  
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

<xsl:template match="/kdoc/file/doc">
   <xsl:apply-templates select="body"/>
   <p><xsl:value-of select="description"/></p>
   <hr/>
</xsl:template>

<xsl:template match="dec/valbind">
   <h3>val <xsl:value-of select="name"/></h3><br />
</xsl:template>

<xsl:template match="dec/fun">
   <h3>fun <xsl:value-of select="name"/></h3><br />
</xsl:template>

<xsl:template match="dec/structure">
   <h2>structure <xsl:value-of select="name"/></h2><br />
   <xsl:apply-templates select="body"/>
</xsl:template>


</xsl:stylesheet>

