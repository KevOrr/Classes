import maya.cmds as cmds

# dictionary of {'object.full_attr': {frame_number: value}}
keys = {'lLeg.translateZ': {1: 9, 11: 9, 21: 27},
        'rLeg.translateZ': {1: 0, 11: 18, 21: 18},
        'center.translateZ': {1: 6, 11: 15, 21: 24},
        'lLeg.translateY': {15: 1},
        'rLeg.translateY': {5: 1},
        'rArm.translateZ': {1: 8, 11: 13, 21: 26},
        'lArm.translateZ': {1: 4, 11: 17, 21: 22}
       }

for full_attr, attr_keys in keys.items():
    for time, value in attr_keys.items():
        cmds.currentTime(time)
        cmds.setAttr(full_attr, value)

        obj, _, attr = full_attr.rpartition('.')
        cmds.setKeyframe(obj, attribute=attr)
