// lvlRatingUtil
function rafAsync() {
    return new Promise(resolve => {
        requestAnimationFrame(resolve); //faster than set time out
    });
}

function checkElement(selector) {
    if (document.querySelector(selector) === null) {
        return rafAsync().then(() => checkElement(selector));
    } else {
        return Promise.resolve(true);
    }
}


function noUiAnnotateLeft(inner, div_height = 460, bg = "red"){
      
      var div = document.createElement("div")
      var span = document.createElement("span")
      span.classList.add("lvl_noUi_lab")
      span.style.backgroundColor = bg;
      inner_span = '<span class = "align_middle">' + inner + '</span>'      
      span.innerHTML = '<span class = "grip_dots"><img src="grip-horizontal-solid.svg" width= "13px;"> </span>' + inner_span; 
      
      
      var arrow = document.createElement("span")
      arrow.innerHTML = "&#10230;"
      div.append(span)
      div.append(arrow)
      div.classList.add("lvl_noUi_handle")
      return div
    }

checkElement('#lvlRate') //use whichever selector you want
.then((element) => {
     
     
     
    const slider = document.querySelector("#lvlRate");
    const handles = document.querySelectorAll("#lvlRate .noUi-handle");
    const origins = slider.getElementsByClassName('noUi-origin');
    sl_height = 450;
    
    handles_index = handles.length  -1
    
    // "Slight", "Moderate", "Severe"
    
    div_no = noUiAnnotateLeft("No Problems", sl_height, bg = "#EFEDF5")
    div_no.style.cursor = "default";
    handles[handles_index].append(div_no);
    origins[handles_index].setAttribute('disabled', true);
    
    div_extreme = noUiAnnotateLeft("Extreme Problems", sl_height, bg = "#6A51A3")
    div_extreme.style.cursor = "default";
    handles[0].append(div_extreme);
    origins[0].setAttribute('disabled', true);
    
    div_slight = noUiAnnotateLeft("Slight Problems", sl_height, bg = "#BCBDDC")
    div_moderate = noUiAnnotateLeft("Moderate Problems", sl_height, bg = "#9E9AC8")
    div_severe = noUiAnnotateLeft("Severe Problems", sl_height, bg = "#807DBA")
    
    
    var next_handle = 0;
    var double_shot = false;
    var init_values = slider.noUiSlider.get().map((i) => parseInt(i));
    
    var dragging = false;
    
    slider.noUiSlider.on("start", function(){
      dragging = true;
    })
    
    slider.noUiSlider.on("end", function(){
      dragging = false;
    })
    
    slider.noUiSlider.on("set", function(){
      
      // when all labels are shown, deprecate function
      if(next_handle == 3){
        
        slider.noUiSlider.off("set")
        slider.noUiSlider.off("start")
        slider.noUiSlider.off("end")
        // rm spinner div in shiny
        document.querySelector("#lvlRate_spinner_container").style.height = 0; 
        document.querySelector("#lvlRate_spinner_container").remove(); 
        return;
      }
        
      var new_vals = slider.noUiSlider.get().map((i) => parseInt(i)) 
        
      var diff = init_values.map(function (num, idx) {
        return num - new_vals[idx];
      }); 
        
        
        // if user drags labels, don't add labels
        if(dragging){
          init_values = new_vals;
          return;
      }
        
        if(next_handle == 2){
          
          if(diff[2]>0){
            handles[handles_index - 3].append(div_severe);
            reset_vals = [0,  new_vals[2], init_values[2], init_values[3], 100];
            init_values = reset_vals;
            next_handle = 3;
            Shiny.setInputValue("next_handle", next_handle);
            slider.noUiSlider.set(reset_vals);
            
            
            // next btn fallback fix
            $( "#nav_btn_nxt" ).show();
            $( "#nav_btn_nxt_tt" ).hide();
            
            return;        
          } 
          
          if(diff[1]<0){
            handles[handles_index - 3].append(div_severe);
            next_handle = 3;
            Shiny.setInputValue("next_handle", next_handle);
            init_values = new_vals;
            
            // next btn fallback fix
            $( "#nav_btn_nxt" ).show();
            $( "#nav_btn_nxt_tt" ).hide();
            
            return;        
          }
        }
        
        if(next_handle === 1){
          
          if(diff[3]>0){
            handles[handles_index - 2].append(div_moderate);
            reset_vals = [0, 1, new_vals[3], init_values[3], 100];
            slider.noUiSlider.set(reset_vals);
            init_values = reset_vals;
            next_handle = 2;
            Shiny.setInputValue("next_handle", next_handle);
            return;        
          } 
          
          if(diff[2]<0){
            handles[handles_index - 2].append(div_moderate);
            next_handle = 2;
            Shiny.setInputValue("next_handle", next_handle);
            init_values = new_vals;
            return;        
          }
        }
        
        if(next_handle === 0){
          handles[handles_index - 1].append(div_slight);
          next_handle = 1;
          Shiny.setInputValue("next_handle", next_handle);
          init_values = new_vals;
          return;        
        }
        
        init_values = new_vals;
      
    })
    
});
