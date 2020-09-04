<template>
  <div class="container containerColor">
    <div id="results" class="column" v-if="notifications.length">
      <div
          v-for="notification in notifications"
          :key="notification.time">
        <Notification :notification="notification">
        </Notification>
        <br>
      </div>
    </div>
    <div v-else id="noMatches">
      <h1>No Notifications found!</h1>
    </div>
    <observer v-on:intersect="$emit('loadMoreNotifications')"></observer>
  </div>
</template>

<script>
import store from "../store";
import toastMixin from "../mixins/toastMixin";
import Observer from "./Observer";
import api from "@/Api";
import Notification from "@/components/Notification";

export default {
  name: "HomeFeed",
  components: {
    Observer,
    Notification
  },
  mixins: [toastMixin],
  data() {
    return {
      store: store,
      observer: null,
      notifications: [],
    }
  },
  methods: {
      loadMoreNotifications(){
        api.getNotifications(this.store.getters.getUserId, localStorage.getItem("authToken")).then(response => {
            if(response.data.length > 0){
              this.notifications = response.data;
            }
        })
      }
  },
  mounted() {
    for (let i = 0; i < 50; i++) {
      let notification = {message: `This is dummy notification #${i}`, notificationType: i};
      this.notifications.push(notification);
    }
  }
}
</script>

<style scoped>

</style>