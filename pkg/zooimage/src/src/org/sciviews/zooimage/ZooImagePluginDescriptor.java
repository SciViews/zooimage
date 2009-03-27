package org.sciviews.zooimage;

import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ZooImagePluginDescriptor {
	private String description ;
	private String version ;
	private Class<?extends ZooImagePlugin> clazz ;
	private String name ;
	private String author ;
	
	private static final Pattern pattern = 
		Pattern.compile( "^ziplugin[.](.*?)[.]" ) ;
	private static final String CONFIG = "/zooimage.config" ; 
	private static final String PREFIX = "org.sciviews.zooimage.plugins." ; 
	
	@SuppressWarnings("unchecked")
	public ZooImagePluginDescriptor( Properties prop, String name){
		this.name = name ;
		this.description=prop.getProperty("ziplugin." + name + ".description" ) ;
		this.version=prop.getProperty("ziplugin." + name + ".version" ) ;
		this.author=prop.getProperty("ziplugin." + name + ".author" ) ;
		try {
			this.clazz = (Class<? extends ZooImagePlugin>) Class.forName( PREFIX + name) ;
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	public static HashMap<String,ZooImagePluginDescriptor> getPlugins( ){
		InputStream config = ZooImagePluginDescriptor.class.getResourceAsStream( CONFIG ) ;
		Properties prop= new Properties() ;
		try {
			prop.load(config) ;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return getPlugins(prop) ;
	}
	
	private static HashMap<String,ZooImagePluginDescriptor> getPlugins( Properties prop ){
		HashMap<String,ZooImagePluginDescriptor> plugins = 
			new HashMap<String,ZooImagePluginDescriptor>() ;
		
		Enumeration<Object> keys = prop.keys() ;
		while( keys.hasMoreElements() ){
			String key = (String) keys.nextElement() ;
			Matcher m = pattern.matcher( key ) ;
			if( !m.find( ) ) continue ;
			String plugin = m.group(1) ;
			if( !plugins.containsKey(plugin) ){
				plugins.put(  plugin, new ZooImagePluginDescriptor( prop, plugin) ) ;
			}
		}
		return plugins ;
	}
	
	public String getDescription() {
		return description;
	}
	
	public String getName() {
		return name;
	}
	
	public String getVersion() {
		return version;
	}
	
	public Class<? extends ZooImagePlugin> getClazz() {
		return clazz;
	}

	public String getAuthor() {
		return author;
	}
	
}
