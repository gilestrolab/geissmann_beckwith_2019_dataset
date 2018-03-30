__author__ = 'quentin'

# Build ROIs from greyscale image
#from ethoscope.roi_builders.target_roi_builder import SleepMonitorWithTargetROIBuilder
from ethoscope.roi_builders.img_roi_builder import ImgMaskROIBuilder
# the robust self learning tracker
from ethoscope.hardware.input.cameras import MovieVirtualCamera
from ethoscope.trackers.adaptive_bg_tracker import AdaptiveBGModel
from ethoscope.core.monitor import Monitor
from ethoscope.stimulators.sleep_depriver_stimulators import IsMovingStimulator



import optparse
import os
import cv2

OUT_DIR = "/data/ethoscope_female_videos/rois/"
IN_AVI_FILE = "/data/ethoscope_female_videos/whole_2016-04-18_14-06-08_035aeeee10184bb39b0754e75cef7900__1920x1080@25_00000.mp4"
REF_ROI_IMG = "/data/ethoscope_female_videos/whole_2016-04-18_14-06-08_035aeeee10184bb39b0754e75cef7900__1920x1080@25_00000.png"

MAX_TIME = 50 * 3600 # 50h
ROI_TO_EXCLUDE = {}


if __name__ == "__main__":

    parser = optparse.OptionParser()

    cam = MovieVirtualCamera(IN_AVI_FILE, use_wall_clock=False)
    #ref = cv2.imread(REF_ROI_IMG)
    roi_builder = ImgMaskROIBuilder(REF_ROI_IMG)

    
#    rois = roi_builder.build(REF_ROI_IMG)
    rois = roi_builder.build(cam)
    rois.sort(key = lambda r: r.get_feature_dict()["x"])
    for i,r in enumerate(rois):
        r.set_value(i+1)

    
    for r in rois:
        if r.value in ROI_TO_EXCLUDE:
            continue

        d = r.get_feature_dict()
        
        out_file_basename = "%s_%02d.mp4" %(os.path.basename(IN_AVI_FILE), d["value"])
        out_file_path = os.path.join(OUT_DIR,out_file_basename)
        print "Generating %s" % out_file_path
        t=0
        command ='ffmpeg  -n -ss %i -i %s   -t %i -vf "crop=%i:%i:%i:%i"   -c:v libx264 -preset slow -qp 18 %s' %(
            t,
            IN_AVI_FILE,
            MAX_TIME,
            d["w"],
            d["h"],
            d["x"],
            d["y"],
            out_file_path
        )
        # print command
        os.system(command)

