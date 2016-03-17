package com.kcorr.planetariumremote;

import android.os.Bundle;
import android.support.design.widget.FloatingActionButton;
import android.support.design.widget.Snackbar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.View;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.DatePicker;
import android.widget.ImageButton;

import com.mhuss.AstroLib.AstroDate;
import com.mhuss.AstroLib.PlanetData;
import com.mhuss.AstroLib.Planets;
import com.mhuss.AstroLib.ObsInfo;

import java.lang.Override;

public class MainActivity extends AppCompatActivity {

    private ImageButton rewindButton;
    private ImageButton forwardButton;
    private DatePicker datePicker;
    private FloatingActionButton sendFab;

    final int[] PLANETS = {Planets.MERCURY, Planets.VENUS, Planets.EARTH};

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
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

        this.rewind = (ImageButton) findViewById(R.id.rewind);
        this.forward = (ImageButton) findViewById(R.id.forward);
        this.datePicker = (DatePicker) findViewById(R.id.datePicker);
        this.sendFab = (FloatingActionButton) findViewById(R.id.send);

        addClickListeners();
    }

    protected void addClickListeners() {

        this.rewindButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                datePicker.updateDate(datePicker.getYear() - 100,
                        datePicker.getMonth(), datePicker.getDayOfMonth());
            }
        });

        this.forwardButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                datePicker.updateDate(datePicker.getYear() + 100,
                        datePicker.getMonth(), datePicker.getDayOfMonth());
            }
        });

        this.sendFab.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                double positions[][] = getHelioPositions(datePicker.getDayOfMonth(),
                        datePicker.getMonth(), datePicker.getYear());
                // TODO send bluetooth
            }
        });
    }

    public static double[][] getHelioPositions(int day, int month, int year) {
        double result[][] = new double[this.PLANETS.length][3];

        double jd = new AstroDate(day, month, year).jd();
        ObsInfo info = new ObsInfo();
        PlanetData data = new PlanetData();

        for (int i=0; i<this.PLANETS.length; i++) {
            data.calc(this.PLANETS[i], jd, info);

            result[i][0] = this.PLANETS[i];
            result[i][1] = data.getPolarLon();
            result[i][2] = data.getPolarLat();
        }

        return result;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}
