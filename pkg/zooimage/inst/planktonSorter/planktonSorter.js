/****** ZooImage plankton sorter JavaScript code, v. 1.0-0 ********************/

// Header scrolling horizontally, but not vertically
function floatHorizontalScroll (id) {
  jQuery(window).scroll(function () {
    jQuery(id).css({
      'left': 0 - jQuery(this).scrollLeft()
    });
  });
};

// Image full-size preview
this.imagePreview = function () {	
  xOffset = -10;
  yOffset = 10;		
    
  $("img.preview").hover(function (e) {
  // this.title = "";									 
  $("body").append("<p id='preview'><img src='"+ this.src +"' alt='"+
    this.alt + "' /><br/>full-size</p>");
  $("#preview")
    .css("top",(e.pageY - xOffset) + "px")
    .css("left",(e.pageX + yOffset) + "px")
    .fadeIn("slow");
},

function () {
  //this.title = this.t;
  $("#preview").remove();
});	

$("img.preview").mousemove(function (e) {
  $("#preview")
    .css("top",(e.pageY - xOffset) + "px")
    .css("left",(e.pageX + yOffset) + "px");
  });			
};

// Kick off the jQuery with the document ready function on page load
$(document).ready(function () {
  floatHorizontalScroll('#header');
  imagePreview();
});

/*******************************************************************************
(C) www.dhtmlgoodies.com, November 2005

Update log:

December 20th, 2005 : Version 1.1: Added support for rectangle indicating where
  object will be dropped
January 11th, 2006: Support for cloning, i.e. "copy & paste" items instead of
  "cut & paste"
January 18th, 2006: Allowing multiple instances to be dragged to same box
  (applies to "cloning mode")

This is a script from www.dhtmlgoodies.com. You will find this and a lot of
other scripts at our website.

    Terms of use:
You are free to use this script as long as the copyright message is kept
intact. However, you may not redistribute, sell or repost it without our
permission.

Thank you!

www.dhtmlgoodies.com
Alf Magne Kalleland

*******************************************************************************/

/* VARIABLES YOU COULD MODIFY */
var boxSize = 200;  // Allow for 200 places for each group
var arrow_offsetX = -5;	// Offset X - position of small arrow
var arrow_offsetY = 0;	// Offset Y - position of small arrow
var arrow_offsetX_firefox = -6;	// Firefox - offset X small arrow
var arrow_offsetY_firefox = -13; // Firefox - offset Y small arrow
var verticalSpaceBetweenListItems = 3;	// Pixels space between a <li> and next
                                        // Same value or higher as margin bottom
                                        // in CSS for #dragDropContainer ul li,
                                        // and #dragContent li
var indicateDestionationByUseOfArrow = false;	// Display arrow to indicate where
                                              // object will be dropped
                                              // (false = use rectangle)
var cloneSourceItems = false;	// Items picked from main container will be cloned
                              // (i.e. "copy" instead of "cut").
var cloneAllowDuplicates = true;	// Allow multiple instances of an item inside
                                  // a small box
                                  // example: drag Vignette 1 to group A twice
/* END VARIABLES YOU COULD MODIFY */

var dragDropTopContainer = false;
var dragTimer = -1;
var dragContentObj = false;
var contentToBeDragged = false;	// Reference to dragged <li>
var contentToBeDragged_src = false;	 // Ref. to parent of <li> before drag
var contentToBeDragged_next = false; // Ref. to next sibling of <li> to drag
var destinationObj = false;	// Ref. to <UL> or <LI> where element is dropped
var dragDropIndicator = false;	// Ref. to small arrow indicating where to drop
var ulPositionArray = new Array();
var mouseoverObj = false;	// Ref. to highlighted DIV

var MSIE = navigator.userAgent.indexOf('MSIE') >= 0 ? true : false;
var navigatorVersion = navigator.appVersion
  .replace(/.*?MSIE (\d\.\d).*/g,'$1')/1;

var indicateDestinationBox = false;

function getTopPos (inputObj) {
  var returnValue = inputObj.offsetTop;
  while((inputObj = inputObj.offsetParent) != null){
    if(inputObj.tagName!='HTML')returnValue += inputObj.offsetTop;
  }
  return returnValue;
}

function getLeftPos (inputObj) {
  var returnValue = inputObj.offsetLeft;
  while((inputObj = inputObj.offsetParent) != null){
    if(inputObj.tagName!='HTML')returnValue += inputObj.offsetLeft;
  }
  return returnValue;
}

function cancelEvent () {
  return false;
}

function initDrag (e) {	// Mouse button is pressed down on a LI
  if(document.all) e = event;
  var st = Math.max(document.body.scrollTop,
    document.documentElement.scrollTop);
  var sl = Math.max(document.body.scrollLeft,
    document.documentElement.scrollLeft);

  dragTimer = 0;
  dragContentObj.style.left = e.clientX + sl + 'px';
  dragContentObj.style.top = e.clientY + st + 'px';
  contentToBeDragged = this;
  contentToBeDragged_src = this.parentNode;
  contentToBeDragged_next = false;
  if (this.nextSibling) {
    contentToBeDragged_next = this.nextSibling;
    if (!this.tagName && contentToBeDragged_next.nextSibling)
      contentToBeDragged_next = contentToBeDragged_next.nextSibling;
  }
  timerDrag();
  return false;
}

function timerDrag() {
  if (dragTimer >= 0 && dragTimer < 10) {
    dragTimer++;
    setTimeout('timerDrag()', 10);
    return;
  }
    
  if (dragTimer == 10) {
    if (cloneSourceItems && contentToBeDragged.parentNode.id == 'allItems') {
      newItem = contentToBeDragged.cloneNode(true);
      newItem.onmousedown = contentToBeDragged.onmousedown;
      contentToBeDragged = newItem;
    }
    dragContentObj.style.display='block';
    dragContentObj.appendChild(contentToBeDragged);
  }
}

function moveDragContent (e) {
  if (dragTimer < 10) {
    if (contentToBeDragged) {
      if (contentToBeDragged_next) {
        contentToBeDragged_src.insertBefore(contentToBeDragged,
          contentToBeDragged_next);
      } else {
        contentToBeDragged_src.appendChild(contentToBeDragged);
      }
    }
    return;
  }
  if (document.all) e = event;
  var st = Math.max(document.body.scrollTop,
    document.documentElement.scrollTop);
  var sl = Math.max(document.body.scrollLeft,
    document.documentElement.scrollLeft);
    
  dragContentObj.style.left = e.clientX + sl + 'px';
  dragContentObj.style.top = e.clientY + st + 'px';

  if (mouseoverObj) mouseoverObj.className = '';
  destinationObj = false;
  dragDropIndicator.style.display = 'none';
  if (indicateDestinationBox) indicateDestinationBox.style.display = 'none';
  var x = e.clientX;
  if (x > 90) x = x + sl;
  var y = e.clientY + st;
  var width = dragContentObj.offsetWidth;
  var height = dragContentObj.offsetHeight;

  var tmpOffsetX = arrow_offsetX;
  var tmpOffsetY = arrow_offsetY;
  if (!document.all) {
    tmpOffsetX = arrow_offsetX_firefox;
    tmpOffsetY = arrow_offsetY_firefox;
  }

  for (var no = 0; no < ulPositionArray.length; no++) {
    var ul_leftPos = ulPositionArray[no]['left'];
    var ul_topPos = ulPositionArray[no]['top'];
    var ul_height = ulPositionArray[no]['height'];
    var ul_width = ulPositionArray[no]['width'];

    if ((x + width) > ul_leftPos && x < (ul_leftPos + ul_width) &&
      (y + height) > ul_topPos && y < (ul_topPos + ul_height)) {
      var noExisting = ulPositionArray[no]['obj']
        .getElementsByTagName('LI').length;
      if (indicateDestinationBox &&
        indicateDestinationBox.parentNode == ulPositionArray[no]['obj'])
        noExisting--;
      if (noExisting < boxSize || no == 0) {
        dragDropIndicator.style.left = ul_leftPos + tmpOffsetX + 'px';
        var subLi = ulPositionArray[no]['obj'].getElementsByTagName('LI');

        var clonedItemAllreadyAdded = false;
        if (cloneSourceItems && !cloneAllowDuplicates) {
          for (var liIndex = 0; liIndex < subLi.length; liIndex++) {
            if (contentToBeDragged.id == subLi[liIndex].id)
              clonedItemAllreadyAdded = true;
          }
          if (clonedItemAllreadyAdded) continue;
        }

        for (var liIndex = 0; liIndex < subLi.length; liIndex++) {
          var tmpTop = getTopPos(subLi[liIndex]);
          if (!indicateDestionationByUseOfArrow) {
            if (y < tmpTop) {
              destinationObj = subLi[liIndex];
              indicateDestinationBox.style.display = 'block';
              subLi[liIndex].parentNode.insertBefore(indicateDestinationBox,
                subLi[liIndex]);
              break;
            }
          } else {
            if (y < tmpTop) {
              destinationObj = subLi[liIndex];
              dragDropIndicator.style.top = tmpTop + tmpOffsetY -
                Math.round(dragDropIndicator.clientHeight / 2) + 'px';
              dragDropIndicator.style.display='block';
              break;
            }
          }
        }

        if (!indicateDestionationByUseOfArrow) {
          if (indicateDestinationBox.style.display == 'none') {
            indicateDestinationBox.style.display = 'block';
            ulPositionArray[no]['obj'].appendChild(indicateDestinationBox);
          }
        } else {
          if (subLi.length > 0 && dragDropIndicator.style.display == 'none') {
            dragDropIndicator.style.top = getTopPos(subLi[subLi.length-1]) +
              subLi[subLi.length-1].offsetHeight + tmpOffsetY + 'px';
            dragDropIndicator.style.display = 'block';
          }
          if (subLi.length == 0) {
            dragDropIndicator.style.top = ul_topPos + arrow_offsetY + 'px'
            dragDropIndicator.style.display = 'block';
          }
        }
        if (!destinationObj) destinationObj = ulPositionArray[no]['obj'];
        mouseoverObj = ulPositionArray[no]['obj'].parentNode;
        mouseoverObj.className = 'mouseover';
        return;
      }
    }
  }
}

// End dragging. Put <LI> into a destination or back to where it came from.
function dragDropEnd (e) {
  if (dragTimer == -1) return;
  if (dragTimer < 10) {
    dragTimer = -1;
    return;
  }
  dragTimer = -1;
  if (document.all) e = event;

  if (cloneSourceItems && (!destinationObj ||
    (destinationObj && (destinationObj.id=='allItems' ||
    destinationObj.parentNode.id=='allItems')))) {
    contentToBeDragged.parentNode.removeChild(contentToBeDragged);
  } else {
    if (destinationObj) {
      if (destinationObj.tagName == 'UL') {
        destinationObj.appendChild(contentToBeDragged);
      } else {
        destinationObj.parentNode.insertBefore(contentToBeDragged,
          destinationObj);
      }
      mouseoverObj.className = '';
      destinationObj = false;
      dragDropIndicator.style.display = 'none';
      if (indicateDestinationBox) {
        indicateDestinationBox.style.display = 'none';
        document.body.appendChild(indicateDestinationBox);
      }
      contentToBeDragged = false;
      return;
    }
    if (contentToBeDragged_next) {
      contentToBeDragged_src.insertBefore(contentToBeDragged,
        contentToBeDragged_next);
    } else {
      contentToBeDragged_src.appendChild(contentToBeDragged);
    }
  }
  contentToBeDragged = false;
  dragDropIndicator.style.display = 'none';
  if (indicateDestinationBox) {
    indicateDestinationBox.style.display = 'none';
    document.body.appendChild(indicateDestinationBox);
  }
  mouseoverObj = false;
}

// Preparing data to be saved
function saveDragDropNodes () {
  var results = document.getElementById('results');
  var saveString = results.name;
  var uls = dragDropTopContainer.getElementsByTagName('UL');
  for (var no = 0; no < uls.length; no++) {	// Looping through all <ul>
    var lis = uls[no].getElementsByTagName('LI');
    for (var no2 = 0; no2 < lis.length; no2++) {
      if (uls[no].id == "_") {
        alert("You cannot leave items in 'Unclassified' zone. Place every " +
          "non-recognizable items into the '[other]' group first before " +
          "saving your validation!");
        return null;
      }
            
      if (saveString.length > 0) saveString = saveString + ";";
      saveString = saveString + uls[no].id + '/' + lis[no2].id;
    }
  }

  // Post these results to the R process
  results.value = saveString;
  document.getElementById('submitForm').submit();
}

function initDragDropScript () {
  dragContentObj = document.getElementById('dragContent');
  dragDropIndicator = document.getElementById('dragDropIndicator');
  dragDropTopContainer = document.getElementById('dragDropContainer');
  document.documentElement.onselectstart = cancelEvent;;
  // Get array containing all <LI>
  var listItems = dragDropTopContainer.getElementsByTagName('LI');	
  var itemHeight = false;
  for (var no = 0; no < listItems.length; no++) {
    listItems[no].onmousedown = initDrag;
    listItems[no].onselectstart = cancelEvent;
    if (!itemHeight) itemHeight = listItems[no].offsetHeight;
    if (MSIE && navigatorVersion/1 < 6) {
      listItems[no].style.cursor='hand';
    }
  }

  var mainContainer = document.getElementById('mainContainer');
  var uls = mainContainer.getElementsByTagName('UL');
  itemHeight = itemHeight + verticalSpaceBetweenListItems;
  for (var no = 0; no < uls.length; no++) {
    uls[no].style.height = itemHeight * boxSize + 'px';
  }

  var leftContainer = document.getElementById('listOfItems');
  var itemBox = leftContainer.getElementsByTagName('UL')[0];

  // Mouse move event - moving draggable div
  document.documentElement.onmousemove = moveDragContent;
  // Mouse move event - moving draggable div
  document.documentElement.onmouseup = dragDropEnd;

  var ulArray = dragDropTopContainer.getElementsByTagName('UL');
  for (var no = 0; no < ulArray.length; no++) {
    ulPositionArray[no] = new Array();
    ulPositionArray[no]['left'] = getLeftPos(ulArray[no]);
    ulPositionArray[no]['top'] = getTopPos(ulArray[no]);
    ulPositionArray[no]['width'] = ulArray[no].offsetWidth;
    ulPositionArray[no]['height'] = ulArray[no].clientHeight;
    ulPositionArray[no]['obj'] = ulArray[no];
  }

  if (!indicateDestionationByUseOfArrow) {
    indicateDestinationBox = document.createElement('LI');
    indicateDestinationBox.id = 'indicateDestination';
    indicateDestinationBox.style.display='none';
    document.body.appendChild(indicateDestinationBox);
  }
}

window.onload = initDragDropScript;
