package com.kcorr.planetariumremote;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Set;


public class DeviceDiscoveryActivity extends AppCompatActivity {

    private final String TAG = "DeviceDiscoveryActivity";

    public static final String RESULT_MAC_ADDR
            = "com.kcorr.planetariumremote.DeviceDiscoveryActivity.RESULT_MAC_ADDR";

    private BluetoothAdapter btAdapter;
    private ArrayList<String[]> deviceList = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_device_discovery);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        btAdapter = BluetoothAdapter.getDefaultAdapter();
        buildList();
    }

    private void buildList() {
        Set<BluetoothDevice> pairedDevices = this.btAdapter.getBondedDevices();
        for (BluetoothDevice device : pairedDevices) {
            String[] deviceInfo = {device.getName(), device.getAddress()};
            this.deviceList.add(deviceInfo);
        }

        DeviceAdapter adapter = new DeviceAdapter(this, deviceList, new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String addr = ((TextView)v.findViewById(R.id.addr)).getText().toString();
                if (BuildConfig.DEBUG) Log.d(TAG, "User selected device with addr " + addr);

                Intent intent = new Intent();
                intent.putExtra(RESULT_MAC_ADDR, addr);
                setResult(RESULT_OK, intent);
                finish();
            }
        });
        ((ListView) findViewById(R.id.device_picker_list)).setAdapter(adapter);
    }

    private class DeviceAdapter extends BaseAdapter {

        private class ViewHolder {
            public TextView nameView;
            public TextView addrView;
        }

        private ArrayList<String[]> deviceList = new ArrayList<>();
        private LayoutInflater inflator;
        private View.OnClickListener listener;

        public DeviceAdapter(Context context, ArrayList<String[]> deviceList, View.OnClickListener listener) {
            this.inflator = (LayoutInflater.from(context));
            this.deviceList = deviceList;
            this.listener = listener;
        }

        @Override
        public int getCount() {
            return this.deviceList.size();
        }

        @Override
        public String[] getItem(int position) {
            return this.deviceList.get(position);
        }

        @Override
        public long getItemId(int position) {
            return position;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolder holder;
            if (convertView == null) {

                convertView = inflator.inflate(R.layout.item_device, null);
                holder = new ViewHolder();
                holder.nameView = (TextView) convertView.findViewById(R.id.name);
                holder.addrView = (TextView) convertView.findViewById(R.id.addr);
                convertView.setTag(holder);
            } else {
                holder = (ViewHolder) convertView.getTag();
            }

            holder.nameView.setText(deviceList.get(position)[0]);
            holder.addrView.setText(deviceList.get(position)[1]);

            convertView.setOnClickListener(listener);

            return convertView;
        }
    }


}
