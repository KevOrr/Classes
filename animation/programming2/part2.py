from maya import cmds

def walk(step=9, swing=2, repeat=1, frames_per_step=10):
    # Describes all keyframes for a single walking cycle
    # Form is {'object.attr': {proportional_time_in_cycle: value}}
    keys = {'lLeg.translateZ':   {0: step,               0.5: step,               1: 3*step            },
            'rLeg.translateZ':   {0: 0,                  0.5: 2*step,             1: 2*step            },
            'center.translateZ': {0: 2.0/3*step,         0.5: 5.0/3*step,         1: 8.0/3*step        },
            'lArm.translateZ':   {0: 2.0/3*step - swing, 0.5: 5.0/3*step + swing, 1: 8.0/3*step - swing},
            'rArm.translateZ':   {0: 2.0/3*step + swing, 0.5: 5.0/3*step - swing, 1: 8.0/3*step + swing},
            'lLeg.translateY':   {            0.25: 0,               0.75: 1                           },
            'rLeg.translateY':   {            0.25: 1,               0.75: 0                           }
    }

    for i in range(repeat):
        for full_attr, attr_keys in keys.items():
            for time_mult, value in attr_keys.items():
                cmds.currentTime((time_mult+i)*frames_per_step + 1)

                obj, _, attr = full_attr.rpartition('.')
                if attr == 'translateZ':
                    value += 2*i*step

                cmds.setAttr(full_attr, value)
                cmds.setKeyframe(obj, full_attr=attr)
