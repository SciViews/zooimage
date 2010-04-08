package org.sciviews.zooimage.gui;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.ImageCanvas;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.sciviews.zooimage.ImageFileProcessor;

@SuppressWarnings("serial")
public class ZooImageGui extends JFrame {

	ImageFileProcessor ifp ;
	
	ImageCanvas canvas ;
	
	public ZooImageGui( ){
		 
		JPanel right = new JPanel( new GridLayout(4, 1) ) ;
		for( int i=0; i<4;i++){
			JPanel p = new JPanel( new BorderLayout() ) ;
			p.add( new JLabel( "image + " + i ) , BorderLayout.NORTH ) ;
			p.add( new JLabel( ""+i ), BorderLayout.CENTER ) ;
			right.add( p, i ) ;
		}
		
		ImagePanel center = new ImagePanel( ) ;
		JSplitPane split = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, center, right ) ;
		split.setDividerLocation(600) ;
		setPreferredSize(new Dimension( 1024, 768)) ;
		add( split ) ;
	}
	
	public void setImageFileProcessor(ImageFileProcessor ifp) {
		this.ifp = ifp;
	}
	
	private class ImagePanel extends JPanel implements AncestorListener, ChangeListener {

		public ImageCanvas canvas ;
		public JScrollPane scroll ;
		
		public ImagePanel(){
			super( new BorderLayout( ) );
			ImagePlus im = IJ.openImage("/home/romain/Desktop/zooimage-exemples/exemples-src/ScanG16-example/ScanG16.2004-10-20+A1.tif") ;
			canvas = new ImageCanvas( im ) ;
			scroll = new JScrollPane( canvas) ;
			scroll.getViewport().addChangeListener(this) ;
			add( scroll );
		}
		
		@Override
		public void ancestorAdded(AncestorEvent event) {}

		@Override
		public void ancestorMoved(AncestorEvent event) {
			canvas.repaint() ;
		}

		@Override
		public void ancestorRemoved(AncestorEvent event) {}

		@Override
		public void stateChanged(ChangeEvent e) {
			SwingUtilities.invokeLater(new Runnable(){
				public void run() {
					canvas.repaint();
				}
			} );
		}
		
		
	}
	
}
