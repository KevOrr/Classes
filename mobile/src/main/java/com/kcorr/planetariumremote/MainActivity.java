package com.kcorr.planetariumremote;

import android.os.Bundle;
import android.support.design.widget.FloatingActionButton;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.DatePicker;
import android.widget.ImageButton;

import com.mhuss.AstroLib.AstroDate;
import com.mhuss.AstroLib.NoInitException;
import com.mhuss.AstroLib.ObsInfo;
import com.mhuss.AstroLib.PlanetData;
import com.mhuss.AstroLib.Planets;

public class MainActivity extends AppCompatActivity {

    private ImageButton rewindButton;
    private ImageButton forwardButton;
    private DatePicker datePicker;
    private FloatingActionButton sendFab;

    private final int[] PLANETS = {Planets.MERCURY, Planets.VENUS, Planets.EARTH};
    private final String TAG = "MainActivity";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        this.rewindButton = (ImageButton) findViewById(R.id.rewind);
        this.forwardButton = (ImageButton) findViewById(R.id.forward);
        this.datePicker = (DatePicker) findViewById(R.id.datePicker);
        this.sendFab = (FloatingActionButton) findViewById(R.id.send);

        addClickListeners();
    }

    private void addClickListeners() {

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
                calcSendPositions();
            }
        });
    }

    private void calcSendPositions() {
        double positions[][] = getHelioPositions(datePicker.getDayOfMonth(),
                datePicker.getMonth(), datePicker.getYear());
        if (BuildConfig.DEBUG) {
            String output = "Got these positions from AstroLib: [";
            for (int i=0; i<positions.length - 1; i++) {
                output += "[" + (int) positions[i][0] + ", " + positions[i][1] + ", "
                        + positions[i][2] + "], ";
            }
            int i = positions.length - 1;
            output += "[" + (int) positions[i][0] + ", " + positions[i][1] + ", "
                    + positions[i][2] + "]]";
            Log.d(TAG, output);
        }

        for (double planet[] : positions) {
            String payload = "" + (int) planet[0] + " " + planet[1] + " " + planet[2];
            if (BuildConfig.DEBUG)
                Log.d(TAG, "Sending this payload: \"" + payload + "\"");
            // TODO send bluetooth
        }
    }


    private double[][] getHelioPositions(int day, int month, int year) {
        double result[][] = new double[this.PLANETS.length][3];

        double jd = new AstroDate(day, month, year).jd();
        ObsInfo info = new ObsInfo();
        PlanetData data = new PlanetData();

        for (int i=0; i<this.PLANETS.length; i++) {
            data.calc(this.PLANETS[i], jd, info);

            try {
                result[i][0] = this.PLANETS[i];
                result[i][1] = data.getPolarLon();
                result[i][2] = data.getPolarLat();
            } catch (NoInitException e) {
                Log.e(TAG, "PlanetData not initiated", e);
            }
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
