package com.kcorr.planetariumremote;

import android.content.Intent;
import android.os.Bundle;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.Snackbar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.View;

import java.util.Arrays;


public class DeviceDiscoveryActivity extends AppCompatActivity {

    public static final String RESULT_MAC_ADDR
            = "com.kcorr.planetariumremote.DeviceDiscoveryActivity.RESULT_MAC_ADDR";
    private static final String ADDR = "98:d3:31:fd:1b:f2";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_device_discovery);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        FloatingActionButton fab = (FloatingActionButton) findViewById(R.id.fab);
        fab.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Snackbar.make(view, "Replace with your own action", Snackbar.LENGTH_LONG)
                        .setAction("Action", null).show();
            }
        });
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        Intent intent = new Intent();
        intent.putExtra(RESULT_MAC_ADDR, ADDR);
        setResult(RESULT_OK, intent);
        finish();
    }
}
